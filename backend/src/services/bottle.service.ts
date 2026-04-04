import * as bottlesDb from '../db/queries/bottles'
import * as usersDb from '../db/queries/users'
import * as containersDb from '../db/queries/containers'
import * as stationsDb from '../db/queries/stations'
import * as routesDb from '../db/queries/routes'
import * as txsDb from '../db/queries/blockchain-txs'
import * as rewardsDb from '../db/queries/rewards'
import * as cardano from './cardano.service'
import { REWARDS_BY_STAGE } from '../db/queries/rewards'
import { validateStage, validateUUID } from '../validate'

/**
 * Cria uma nova garrafa: gera nome automatico, verifica capacidade do container,
 * submete tx de mint e registra no banco.
 * O UTxO sera preenchido pelo confirmation worker apos confirmacao on-chain.
 */
export async function create(params: {
  userId: string
  containerId: string
  volumeMl: number
}) {
  const userId = validateUUID(params.userId)
  const containerId = validateUUID(params.containerId)

  const user = await usersDb.findById(userId)
  if (!user) throw new Error(`Usuario nao encontrado: ${userId}`)

  const container = await containersDb.findById(containerId)
  if (!container) throw new Error(`Container nao encontrado: ${containerId}`)

  // Verifica capacidade do container (converte ml para litros)
  const volumeLiters = params.volumeMl / 1000
  const newVolume = container.current_volume_liters + volumeLiters
  if (newVolume > container.capacity_liters) {
    throw new Error(
      `Container sem capacidade: ${container.current_volume_liters}/${container.capacity_liters}L ` +
      `(tentando adicionar ${params.volumeMl}ml)`,
    )
  }

  // Gera nome automatico: garrafa-XXXX
  const nextNum = await bottlesDb.nextNumber()
  const bottleId = `garrafa-${String(nextNum).padStart(4, '0')}`
  const bottleHex = Buffer.from(bottleId).toString('hex')

  // 1. Submete tx na Cardano
  const txHash = await cardano.createBottle({
    bottleId,
    userAddr: user.wallet_address,
    userPubkeyHash: user.pubkey_hash,
  })

  // 2. Registra a garrafa no banco (utxo ainda nao confirmado)
  const bottle = await bottlesDb.create({
    user_id: userId,
    container_id: containerId,
    bottle_id_text: bottleId,
    bottle_id_hex: bottleHex,
    volume_ml: params.volumeMl,
  })

  // 3. Atualiza volume do container
  await containersDb.updateVolume(containerId, newVolume)
  if (newVolume >= container.capacity_liters && container.status === 'active') {
    await containersDb.updateStatus(containerId, 'full')
  }

  // 4. Registra a tx como pending
  const datumJson = JSON.stringify({
    "constructor": 0,
    "fields": [
      { "bytes": user.pubkey_hash },
      { "bytes": bottleHex },
      { "constructor": 0, "fields": [] },
    ],
  })

  await txsDb.create({
    bottle_id: bottle.id,
    stage: 'inserted',
    tx_hash: txHash,
    datum_json: datumJson,
  })

  return { bottle, txHash }
}

/**
 * Compacta todas as garrafas 'inserted' de um container.
 * Container precisa estar >= 90% cheio.
 * Submete tx de avanco para cada garrafa.
 */
export async function compactContainer(containerId: string) {
  const id = validateUUID(containerId)

  const container = await containersDb.findById(id)
  if (!container) throw new Error(`Container nao encontrado: ${id}`)

  const fillPercent = (container.current_volume_liters / container.capacity_liters) * 100
  if (fillPercent < 90) {
    throw new Error(
      `Container precisa estar pelo menos 90% cheio para compactar. Atual: ${fillPercent.toFixed(1)}%`,
    )
  }

  const bottles = await bottlesDb.findByContainerIdAndStage(id, 'inserted')
  if (bottles.length === 0) {
    throw new Error('Nenhuma garrafa inserted encontrada neste container')
  }

  // Submete tx de avanco para cada garrafa
  const results: { bottleId: string; txHash: string }[] = []
  for (const bottle of bottles) {
    if (!bottle.utxo_hash || bottle.utxo_index === null) continue

    const user = await usersDb.findById(bottle.user_id)
    if (!user) continue

    try {
      const txHash = await cardano.advanceStage({
        bottleId: bottle.bottle_id_text,
        targetStage: 'compacted',
        userAddr: user.wallet_address,
        utxoHash: bottle.utxo_hash,
        utxoIndex: bottle.utxo_index,
      })

      await txsDb.create({
        bottle_id: bottle.id,
        stage: 'compacted',
        tx_hash: txHash,
      })

      await bottlesDb.clearUtxo(bottle.id)
      results.push({ bottleId: bottle.id, txHash })
    } catch (err) {
      console.error(`[compact] Erro ao avancar garrafa ${bottle.id}:`, err)
    }
  }

  // Atualiza estagio em batch no banco
  const count = await bottlesDb.compactByContainer(id)

  return { containerId: id, compacted: count, txs: results }
}

/**
 * Coleta um container (parada de rota): move garrafas compacted -> collected,
 * associa ao route, limpa container.
 * Todas as garrafas devem estar compactadas antes da coleta.
 */
export async function collectContainer(containerId: string, routeId: string) {
  const cId = validateUUID(containerId)
  const rId = validateUUID(routeId)

  const container = await containersDb.findById(cId)
  if (!container) throw new Error(`Container nao encontrado: ${cId}`)

  const route = await routesDb.findById(rId)
  if (!route) throw new Error(`Rota nao encontrada: ${rId}`)

  // Verifica se todas as garrafas do container estao compactadas
  const insertedBottles = await bottlesDb.findByContainerIdAndStage(cId, 'inserted')
  if (insertedBottles.length > 0) {
    throw new Error(
      `Container ainda tem ${insertedBottles.length} garrafa(s) nao compactada(s). Compacte antes de coletar.`,
    )
  }

  const bottles = await bottlesDb.findByContainerIdAndStage(cId, 'compacted')
  if (bottles.length === 0) {
    throw new Error('Nenhuma garrafa compactada encontrada neste container')
  }

  // Submete tx de avanco para cada garrafa
  const results: { bottleId: string; txHash: string }[] = []
  for (const bottle of bottles) {
    if (!bottle.utxo_hash || bottle.utxo_index === null) continue

    const user = await usersDb.findById(bottle.user_id)
    if (!user) continue

    try {
      const txHash = await cardano.advanceStage({
        bottleId: bottle.bottle_id_text,
        targetStage: 'collected',
        userAddr: user.wallet_address,
        utxoHash: bottle.utxo_hash,
        utxoIndex: bottle.utxo_index,
      })

      await txsDb.create({
        bottle_id: bottle.id,
        stage: 'collected',
        tx_hash: txHash,
      })

      await bottlesDb.clearUtxo(bottle.id)
      results.push({ bottleId: bottle.id, txHash })
    } catch (err) {
      console.error(`[collect] Erro ao avancar garrafa ${bottle.id}:`, err)
    }
  }

  // Batch: move garrafas para a rota e marca como collected
  const count = await bottlesDb.collectByContainer(cId, rId)

  // Reseta container
  await containersDb.updateVolume(cId, 0)
  await containersDb.updateStatus(cId, 'active')

  return { containerId: cId, routeId: rId, collected: count, txs: results }
}

/**
 * Entrega garrafas de uma rota em uma estacao de tratamento.
 * Move todas as garrafas 'collected' da rota para a estacao (atstation).
 */
export async function deliverToStation(routeId: string, stationId: string) {
  const rId = validateUUID(routeId)
  const sId = validateUUID(stationId)

  const route = await routesDb.findById(rId)
  if (!route) throw new Error(`Rota nao encontrada: ${rId}`)

  const station = await stationsDb.findById(sId)
  if (!station) throw new Error(`Estacao nao encontrada: ${sId}`)

  const bottles = await bottlesDb.findByRouteId(rId)
  const collectedBottles = bottles.filter(b => b.current_stage === 'collected')
  if (collectedBottles.length === 0) {
    throw new Error('Nenhuma garrafa collected encontrada nesta rota')
  }

  // Submete tx de avanco para cada garrafa
  const results: { bottleId: string; txHash: string }[] = []
  for (const bottle of collectedBottles) {
    if (!bottle.utxo_hash || bottle.utxo_index === null) continue

    const user = await usersDb.findById(bottle.user_id)
    if (!user) continue

    try {
      const txHash = await cardano.advanceStage({
        bottleId: bottle.bottle_id_text,
        targetStage: 'atstation',
        userAddr: user.wallet_address,
        utxoHash: bottle.utxo_hash,
        utxoIndex: bottle.utxo_index,
      })

      await txsDb.create({
        bottle_id: bottle.id,
        stage: 'atstation',
        tx_hash: txHash,
      })

      await bottlesDb.clearUtxo(bottle.id)
      results.push({ bottleId: bottle.id, txHash })
    } catch (err) {
      console.error(`[deliver] Erro ao avancar garrafa ${bottle.id}:`, err)
    }
  }

  // Batch: move garrafas para a estacao
  const count = await bottlesDb.deliverByRoute(rId, sId)

  return { routeId: rId, stationId: sId, delivered: count, txs: results }
}

/**
 * Tritura uma garrafa na estacao de tratamento (atstation -> shredded).
 */
export async function shredBottle(bottleId: string) {
  const id = validateUUID(bottleId)

  const bottle = await bottlesDb.findById(id)
  if (!bottle) throw new Error(`Garrafa nao encontrada: ${id}`)

  if (bottle.current_stage !== 'atstation') {
    throw new Error(
      `Garrafa precisa estar em 'atstation' para triturar. Estagio atual: '${bottle.current_stage}'`,
    )
  }

  if (!bottle.station_id) {
    throw new Error('Garrafa nao esta associada a nenhuma estacao')
  }

  const user = await usersDb.findById(bottle.user_id)
  if (!user) throw new Error(`Usuario da garrafa nao encontrado: ${bottle.user_id}`)

  let txHash: string | null = null

  if (bottle.utxo_hash && bottle.utxo_index !== null) {
    txHash = await cardano.advanceStage({
      bottleId: bottle.bottle_id_text,
      targetStage: 'shredded',
      userAddr: user.wallet_address,
      utxoHash: bottle.utxo_hash,
      utxoIndex: bottle.utxo_index,
    })

    await txsDb.create({
      bottle_id: bottle.id,
      stage: 'shredded',
      tx_hash: txHash,
    })

    await bottlesDb.clearUtxo(bottle.id)
  }

  await bottlesDb.shred(bottle.id)

  return { bottle, txHash }
}

/**
 * Tritura todas as garrafas atstation de uma estacao de tratamento.
 * Submete txs on-chain para cada garrafa (com tolerancia a falhas),
 * depois atualiza o banco em batch.
 */
export async function shredStation(stationId: string) {
  const sId = validateUUID(stationId)

  const station = await stationsDb.findById(sId)
  if (!station) throw new Error(`Estacao nao encontrada: ${sId}`)

  const bottles = await bottlesDb.findByStationId(sId)
  const atstationBottles = bottles.filter(b => b.current_stage === 'atstation')
  if (atstationBottles.length === 0) {
    throw new Error('Nenhuma garrafa atstation encontrada nesta estacao')
  }

  // Submete tx de avanco para cada garrafa (tolerante a falhas)
  const results: { bottleId: string; txHash: string }[] = []
  for (const bottle of atstationBottles) {
    if (!bottle.utxo_hash || bottle.utxo_index === null) continue

    const user = await usersDb.findById(bottle.user_id)
    if (!user) continue

    try {
      const txHash = await cardano.advanceStage({
        bottleId: bottle.bottle_id_text,
        targetStage: 'shredded',
        userAddr: user.wallet_address,
        utxoHash: bottle.utxo_hash,
        utxoIndex: bottle.utxo_index,
      })

      await txsDb.create({
        bottle_id: bottle.id,
        stage: 'shredded',
        tx_hash: txHash,
      })

      await bottlesDb.clearUtxo(bottle.id)
      results.push({ bottleId: bottle.id, txHash })
    } catch (err) {
      console.error(`[shred] Erro ao avancar garrafa ${bottle.id}:`, err)
    }
  }

  // Batch: atualiza todas as garrafas no banco
  const count = await bottlesDb.shredByStation(sId)

  return { stationId: sId, shredded: count, txs: results }
}

/**
 * Consulta o historico completo de uma garrafa (txs + rewards).
 */
export async function getHistory(bottleId: string) {
  const id = validateUUID(bottleId)
  const bottle = await bottlesDb.findById(id)
  if (!bottle) throw new Error(`Garrafa nao encontrada: ${id}`)

  const txs = await txsDb.findByBottleId(id)
  const rewards = await rewardsDb.findByBottleId(id)

  return { bottle, txs, rewards }
}
