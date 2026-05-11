import * as bottlesDb from '../db/queries/bottles'
import * as usersDb from '../db/queries/users'
import * as containersDb from '../db/queries/containers'
import * as stationsDb from '../db/queries/stations'
import * as routesDb from '../db/queries/routes'
import * as txsDb from '../db/queries/blockchain-txs'
import * as rewardsDb from '../db/queries/rewards'
import * as cardano from './cardano.service'
import { validateUUID } from '../validate'

// Compactação reduz a garrafa em 50% do volume original (fixo).
const COMPACTION_REDUCTION = 0.5
const COMPACTED_RATIO = 1 - COMPACTION_REDUCTION

/**
 * Cria uma nova garrafa: gera nome automatico, verifica capacidade do container,
 * submete tx de mint e registra no banco.
 * O UTxO sera preenchido pelo confirmation worker após confirmação on-chain.
 */
export async function create(params: {
  userId: string
  containerId: string
  volumeMl: number
}) {
  const userId = validateUUID(params.userId)
  const containerId = validateUUID(params.containerId)

  const user = usersDb.requireWallet(await usersDb.findById(userId), userId)

  const container = await containersDb.findById(containerId)
  if (!container) throw new Error(`Container não encontrado: ${containerId}`)

  // Valida contra o volume *após* a compactação (50% do original): o container
  // só ocupa o volume final quando a tx de compactação confirma; durante a fase
  // 'inserted' o volume armazenado não muda.
  const volumeLiters = params.volumeMl / 1000
  const projectedAfterCompaction =
    container.current_volume_liters + volumeLiters * COMPACTED_RATIO
  if (projectedAfterCompaction > container.capacity_liters) {
    const used = container.current_volume_liters.toFixed(2)
    const cap = container.capacity_liters.toFixed(2)
    const maxBottleMl = Math.floor(
      (container.capacity_liters - container.current_volume_liters) / COMPACTED_RATIO * 1000,
    )
    throw new Error(
      `Container sem capacidade suficiente: ${used}L de ${cap}L ocupados. ` +
      `Esta garrafa de ${params.volumeMl}ml não cabe nem após compactação ` +
      `(máximo permitido agora: ${Math.max(0, maxBottleMl)}ml).`,
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

  // 2. Registra a garrafa no banco (utxo ainda não confirmado)
  const bottle = await bottlesDb.create({
    user_id: userId,
    container_id: containerId,
    bottle_id_text: bottleId,
    bottle_id_hex: bottleHex,
    volume_ml: params.volumeMl,
  })

  // 3. Registra a tx como pending
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

  // O volume do container só é atualizado quando a tx de compactação confirma
  // (autoCompactBottle), garantindo que ele reflete o estado real (compactado).
  return { bottle, txHash }
}

/**
 * Compacta automaticamente uma garrafa individual logo após a validação on-chain
 * da inserção. Submete a tx de avanço inserted -> compacted no smart contract,
 * registra o blockchain_tx pendente e limpa o UTxO atual da garrafa (será
 * preenchido novamente pelo worker quando a tx de compactação for confirmada).
 */
export async function autoCompactBottle(bottleId: string, operatorTxIn?: string) {
  const id = validateUUID(bottleId)

  const bottle = await bottlesDb.findById(id)
  if (!bottle) throw new Error(`Garrafa não encontrada: ${id}`)
  if (bottle.current_stage !== 'inserted') return null
  if (!bottle.utxo_hash || bottle.utxo_index === null) return null

  const user = usersDb.requireWallet(await usersDb.findById(bottle.user_id), bottle.user_id)

  const txHash = await cardano.advanceStage({
    bottleId: bottle.bottle_id_text,
    targetStage: 'compacted',
    userAddr: user.wallet_address,
    utxoHash: bottle.utxo_hash,
    utxoIndex: bottle.utxo_index,
    operatorTxIn,
  })

  await txsDb.create({
    bottle_id: bottle.id,
    stage: 'compacted',
    tx_hash: txHash,
  })

  // Ex.: 2000ml com redução de 50% -> 1000ml.
  const compactedVolumeMl = bottle.volume_ml * COMPACTED_RATIO
  const compactedLiters = compactedVolumeMl / 1000

  await bottlesDb.updateVolume(bottle.id, compactedVolumeMl)

  // Volume do container só é incrementado agora (após compactar). Antes da
  // compactação o container não reflete a garrafa recém-inserida.
  if (bottle.container_id) {
    const container = await containersDb.findById(bottle.container_id)
    if (container) {
      const newVolume = container.current_volume_liters + compactedLiters
      await containersDb.updateVolume(bottle.container_id, newVolume)
      const fillPercent = (newVolume / container.capacity_liters) * 100
      if (fillPercent >= 90 && container.status === 'active') {
        await containersDb.updateStatus(bottle.container_id, 'ready_for_collection')
      }
    }
  }

  // UTxO antigo foi consumido pela tx; novo UTxO sera preenchido apos confirmacao.
  await bottlesDb.clearUtxo(bottle.id)

  return { bottleId: bottle.id, txHash, compactedVolumeMl, reduction: COMPACTION_REDUCTION }
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
  if (!container) throw new Error(`Container não encontrado: ${cId}`)

  const route = await routesDb.findById(rId)
  if (!route) throw new Error(`Rota não encontrada: ${rId}`)

  // Verifica se todas as garrafas do container ja foram compactadas automaticamente
  const insertedBottles = await bottlesDb.findByContainerIdAndStage(cId, 'inserted')
  if (insertedBottles.length > 0) {
    throw new Error(
      `Container ainda tem ${insertedBottles.length} garrafa(s) aguardando compactação automática. Tente novamente em alguns segundos.`,
    )
  }

  const bottles = await bottlesDb.findByContainerIdAndStage(cId, 'compacted')
  if (bottles.length === 0) {
    throw new Error('Nenhuma garrafa compactada encontrada neste container')
  }

  // Verifica se todas as garrafas tem UTxO confirmado on-chain
  const readyBottles = bottles.filter(b => b.utxo_hash && b.utxo_index !== null)
  if (readyBottles.length < bottles.length) {
    const pending = bottles.length - readyBottles.length
    throw new Error(
      `${pending} garrafa(s) aguardando confirmação on-chain... Tente novamente em alguns segundos.`,
    )
  }

  // Pre-aloca UTxOs do operador (um por garrafa) para evitar contencao
  const operatorUtxos = await cardano.allocateOperatorUtxos(readyBottles.length, 4_000_000)
  if (operatorUtxos.length === 0) {
    throw new Error('Nenhum UTxO do operador disponível para submeter transações')
  }

  // Submete tx de avanco para cada garrafa
  const results: { bottleId: string; txHash: string }[] = []
  for (let i = 0; i < readyBottles.length; i++) {
    const bottle = readyBottles[i]
    if (i >= operatorUtxos.length) {
      console.warn(`[collect] Sem UTxO do operador disponível para garrafa ${bottle.id} (${i + 1}/${readyBottles.length}). Processe novamente após confirmação.`)
      break
    }

    const userRow = await usersDb.findById(bottle.user_id)
    if (!userRow || !userRow.wallet_address) continue
    const user = userRow as usersDb.UserWithWallet

    try {
      const txHash = await cardano.advanceStage({
        bottleId: bottle.bottle_id_text,
        targetStage: 'collected',
        userAddr: user.wallet_address,
        utxoHash: bottle.utxo_hash!,
        utxoIndex: bottle.utxo_index!,
        operatorTxIn: operatorUtxos[i].txIn,
      })

      await txsDb.create({
        bottle_id: bottle.id,
        stage: 'collected',
        tx_hash: txHash,
      })

      await bottlesDb.clearUtxo(bottle.id)
      results.push({ bottleId: bottle.id, txHash })
    } catch (err) {
      console.error(`[collect] Erro ao avançar garrafa ${bottle.id}:`, err)
    }
  }

  // Batch: move apenas garrafas com tx submetida para a rota
  const successIds = results.map(r => r.bottleId)
  const count = await bottlesDb.collectByIds(successIds, rId)

  // Reseta container
  await containersDb.updateVolume(cId, 0)
  await containersDb.updateStatus(cId, 'active')

  return { containerId: cId, routeId: rId, collected: count, txs: results }
}

/**
 * Entrega garrafas de uma rota em uma estação de tratamento.
 * Move todas as garrafas 'collected' da rota para a estação (atstation).
 */
export async function deliverToStation(routeId: string, stationId: string) {
  const rId = validateUUID(routeId)
  const sId = validateUUID(stationId)

  const route = await routesDb.findById(rId)
  if (!route) throw new Error(`Rota não encontrada: ${rId}`)

  const station = await stationsDb.findById(sId)
  if (!station) throw new Error(`estação não encontrada: ${sId}`)

  const bottles = await bottlesDb.findByRouteId(rId)
  const collectedBottles = bottles.filter(b => b.current_stage === 'collected')
  if (collectedBottles.length === 0) {
    throw new Error('Nenhuma garrafa coletada encontrada nesta rota')
  }

  // Verifica se todas as garrafas tem UTxO confirmado on-chain
  const readyBottles = collectedBottles.filter(b => b.utxo_hash && b.utxo_index !== null)
  if (readyBottles.length < collectedBottles.length) {
    const pending = collectedBottles.length - readyBottles.length
    throw new Error(
      `${pending} garrafa(s) aguardando confirmação on-chain... Tente novamente em alguns segundos.`,
    )
  }

  // Pre-aloca UTxOs do operador (um por garrafa) para evitar contencao
  const operatorUtxos = await cardano.allocateOperatorUtxos(readyBottles.length, 4_000_000)
  if (operatorUtxos.length === 0) {
    throw new Error('Nenhum UTxO do operador disponível para submeter transações')
  }

  // Submete tx de avanco para cada garrafa
  const results: { bottleId: string; txHash: string }[] = []
  for (let i = 0; i < readyBottles.length; i++) {
    const bottle = readyBottles[i]
    if (i >= operatorUtxos.length) {
      console.warn(`[deliver] Sem UTxO do operador disponível para garrafa ${bottle.id} (${i + 1}/${readyBottles.length}). Processe novamente após confirmação.`)
      break
    }

    const userRow = await usersDb.findById(bottle.user_id)
    if (!userRow || !userRow.wallet_address) continue
    const user = userRow as usersDb.UserWithWallet

    try {
      const txHash = await cardano.advanceStage({
        bottleId: bottle.bottle_id_text,
        targetStage: 'atstation',
        userAddr: user.wallet_address,
        utxoHash: bottle.utxo_hash!,
        utxoIndex: bottle.utxo_index!,
        operatorTxIn: operatorUtxos[i].txIn,
      })

      await txsDb.create({
        bottle_id: bottle.id,
        stage: 'atstation',
        tx_hash: txHash,
      })

      await bottlesDb.clearUtxo(bottle.id)
      results.push({ bottleId: bottle.id, txHash })
    } catch (err) {
      console.error(`[deliver] Erro ao avançar garrafa ${bottle.id}:`, err)
    }
  }

  // Batch: move apenas garrafas com tx submetida para a estação
  const successIds = results.map(r => r.bottleId)
  const count = await bottlesDb.deliverByIds(successIds, sId)

  return { routeId: rId, stationId: sId, delivered: count, txs: results }
}

/**
 * Tritura uma garrafa na estação de tratamento (atstation -> shredded).
 */
export async function shredBottle(bottleId: string) {
  const id = validateUUID(bottleId)

  const bottle = await bottlesDb.findById(id)
  if (!bottle) throw new Error(`Garrafa não encontrada: ${id}`)

  if (bottle.current_stage !== 'atstation') {
    throw new Error(
      `Garrafa precisa estar em 'atstation' para triturar. Estágio atual: '${bottle.current_stage}'`,
    )
  }

  if (!bottle.station_id) {
    throw new Error('Garrafa não esta associada a nenhuma estação')
  }

  const user = usersDb.requireWallet(await usersDb.findById(bottle.user_id), bottle.user_id)

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
 * Tritura todas as garrafas atstation de uma estação de tratamento.
 * Submete txs on-chain para cada garrafa (com tolerancia a falhas),
 * depois atualiza o banco em batch.
 */
export async function shredStation(stationId: string) {
  const sId = validateUUID(stationId)

  const station = await stationsDb.findById(sId)
  if (!station) throw new Error(`estação não encontrada: ${sId}`)

  const bottles = await bottlesDb.findByStationId(sId)
  const atstationBottles = bottles.filter(b => b.current_stage === 'atstation')
  if (atstationBottles.length === 0) {
    throw new Error('Nenhuma garrafa na estação encontrada nesta estação')
  }

  // Verifica se todas as garrafas tem UTxO confirmado on-chain
  const readyBottles = atstationBottles.filter(b => b.utxo_hash && b.utxo_index !== null)
  if (readyBottles.length < atstationBottles.length) {
    const pending = atstationBottles.length - readyBottles.length
    throw new Error(
      `${pending} garrafa(s) aguardando confirmação on-chain... Tente novamente em alguns segundos.`,
    )
  }

  // Pre-aloca UTxOs do operador (um por garrafa) para evitar contencao
  const operatorUtxos = await cardano.allocateOperatorUtxos(readyBottles.length, 4_000_000)
  if (operatorUtxos.length === 0) {
    throw new Error('Nenhum UTxO do operador disponível para submeter transações')
  }

  // Submete tx de avanco para cada garrafa (tolerante a falhas)
  const results: { bottleId: string; txHash: string }[] = []
  for (let i = 0; i < readyBottles.length; i++) {
    const bottle = readyBottles[i]
    if (i >= operatorUtxos.length) {
      console.warn(`[shred] Sem UTxO do operador disponível para garrafa ${bottle.id} (${i + 1}/${readyBottles.length}). Processe novamente após confirmação.`)
      break
    }

    const userRow = await usersDb.findById(bottle.user_id)
    if (!userRow || !userRow.wallet_address) continue
    const user = userRow as usersDb.UserWithWallet

    try {
      const txHash = await cardano.advanceStage({
        bottleId: bottle.bottle_id_text,
        targetStage: 'shredded',
        userAddr: user.wallet_address,
        utxoHash: bottle.utxo_hash!,
        utxoIndex: bottle.utxo_index!,
        operatorTxIn: operatorUtxos[i].txIn,
      })

      await txsDb.create({
        bottle_id: bottle.id,
        stage: 'shredded',
        tx_hash: txHash,
      })

      await bottlesDb.clearUtxo(bottle.id)
      results.push({ bottleId: bottle.id, txHash })
    } catch (err) {
      console.error(`[shred] Erro ao avançar garrafa ${bottle.id}:`, err)
    }
  }

  // Batch: atualiza apenas garrafas com tx submetida
  const successIds = results.map(r => r.bottleId)
  const count = await bottlesDb.shredByIds(successIds)

  return { stationId: sId, shredded: count, txs: results }
}

/**
 * Consulta o historico completo de uma garrafa (txs + rewards).
 */
export async function getHistory(bottleId: string) {
  const id = validateUUID(bottleId)
  const bottle = await bottlesDb.findById(id)
  if (!bottle) throw new Error(`Garrafa não encontrada: ${id}`)

  const txs = await txsDb.findByBottleId(id)
  const rewards = await rewardsDb.findByBottleId(id)

  return { bottle, txs, rewards }
}
