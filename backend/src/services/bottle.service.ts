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

// Mutex global para a fase critica do create(): consulta volume + pendentes,
// seleciona proximo numero e insere a linha no DB. Sem isso, duas chamadas
// concorrentes podem (a) gerar o mesmo bottle_id_text e (b) ambas passarem na
// checagem de capacidade pois nenhuma ainda enxerga a garrafa em voo da outra.
// O trecho lento (blockchain) corre fora do lock - o pre-insert ja reserva o
// numero e ja conta na contagem de "inserted" para a proxima chamada.
let createLock: Promise<void> = Promise.resolve()
async function acquireCreateLock(): Promise<() => void> {
  let release!: () => void
  const next = new Promise<void>((r) => { release = r })
  const prev = createLock
  createLock = next
  await prev
  return release
}

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

  // Secao critica: capacidade + reserva de numero + insert atomicamente.
  // A checagem inclui pending (inserted ainda nao compactadas) para evitar que
  // varias garrafas concorrentes passem cada uma na propria checagem mas o
  // somatorio extrapole a capacidade do container.
  const release = await acquireCreateLock()
  let bottle: bottlesDb.Bottle
  let bottleId: string
  let bottleHex: string
  try {
    const container = await containersDb.findById(containerId)
    if (!container) throw new Error(`Container não encontrado: ${containerId}`)

    const pendingLiters = await bottlesDb.pendingInsertedLiters(containerId)
    const volumeLiters = params.volumeMl / 1000
    const projectedAfterCompaction =
      container.current_volume_liters + pendingLiters + volumeLiters * COMPACTED_RATIO
    if (projectedAfterCompaction > container.capacity_liters) {
      const used = container.current_volume_liters.toFixed(2)
      const cap = container.capacity_liters.toFixed(2)
      const availableLiters = Math.max(
        0,
        container.capacity_liters - container.current_volume_liters - pendingLiters,
      )
      const maxBottleMl = Math.floor((availableLiters / COMPACTED_RATIO) * 1000)
      throw new Error(
        `Container sem capacidade suficiente: ${used}L de ${cap}L ocupados ` +
        `(+ ${pendingLiters.toFixed(2)}L em garrafas pendentes). ` +
        `Esta garrafa de ${params.volumeMl}ml não cabe nem após compactação ` +
        `(máximo permitido agora: ${Math.max(0, maxBottleMl)}ml).`,
      )
    }

    // Gera nome automatico: garrafa-XXXX (sob lock, evita colisao)
    const nextNum = await bottlesDb.nextNumber()
    bottleId = `garrafa-${String(nextNum).padStart(4, '0')}`
    bottleHex = Buffer.from(bottleId).toString('hex')

    // Pre-insere a garrafa: ja conta como 'inserted' para a proxima chamada
    // calcular pendingInsertedLiters corretamente.
    bottle = await bottlesDb.create({
      user_id: userId,
      container_id: containerId,
      bottle_id_text: bottleId,
      bottle_id_hex: bottleHex,
      volume_ml: params.volumeMl,
    })
  } finally {
    release()
  }

  // Submete tx na Cardano (fora do lock - parte lenta). Em caso de erro,
  // remove a garrafa pre-inserida para nao deixar lixo bloqueando capacidade.
  let txHash: string
  try {
    txHash = await cardano.createBottle({
      bottleId,
      userAddr: user.wallet_address,
      userPubkeyHash: user.pubkey_hash,
    })
  } catch (err) {
    await bottlesDb.deleteById(bottle.id).catch(() => {})
    throw err
  }

  // Registra a tx como pending
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

  // Idempotência: se já existe tx pendente para a garrafa indo para `collected`,
  // reusa em vez de submeter novamente (clique duplo / retry do operador).
  const pendingByBottle = new Map<string, txsDb.BlockchainTx>()
  const needsSubmit: typeof readyBottles = []
  for (const bottle of readyBottles) {
    const existing = await txsDb.findPendingByBottleAndStage(bottle.id, 'collected')
    if (existing) {
      pendingByBottle.set(bottle.id, existing)
    } else {
      needsSubmit.push(bottle)
    }
  }

  // Aloca UTxOs do operador apenas para as garrafas que realmente vão submeter
  const operatorUtxos = needsSubmit.length > 0
    ? await cardano.allocateOperatorUtxos(needsSubmit.length, 4_000_000)
    : []
  if (needsSubmit.length > 0 && operatorUtxos.length === 0) {
    throw new Error('Nenhum UTxO do operador disponível para submeter transações')
  }

  // Resultado começa com as garrafas que já tinham tx pendente
  const results: { bottleId: string; txHash: string }[] = []
  for (const [bottleId, tx] of pendingByBottle) {
    if (tx.tx_hash) results.push({ bottleId, txHash: tx.tx_hash })
  }

  // Submete tx de avanco para cada garrafa nova
  for (let i = 0; i < needsSubmit.length; i++) {
    const bottle = needsSubmit[i]
    if (i >= operatorUtxos.length) {
      console.warn(`[collect] Sem UTxO do operador disponível para garrafa ${bottle.id} (${i + 1}/${needsSubmit.length}). Processe novamente após confirmação.`)
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

  // Idempotência: reusa txs pendentes para `atstation` em vez de duplicar submit
  const pendingByBottle = new Map<string, txsDb.BlockchainTx>()
  const needsSubmit: typeof readyBottles = []
  for (const bottle of readyBottles) {
    const existing = await txsDb.findPendingByBottleAndStage(bottle.id, 'atstation')
    if (existing) {
      pendingByBottle.set(bottle.id, existing)
    } else {
      needsSubmit.push(bottle)
    }
  }

  const operatorUtxos = needsSubmit.length > 0
    ? await cardano.allocateOperatorUtxos(needsSubmit.length, 4_000_000)
    : []
  if (needsSubmit.length > 0 && operatorUtxos.length === 0) {
    throw new Error('Nenhum UTxO do operador disponível para submeter transações')
  }

  const results: { bottleId: string; txHash: string }[] = []
  for (const [bottleId, tx] of pendingByBottle) {
    if (tx.tx_hash) results.push({ bottleId, txHash: tx.tx_hash })
  }

  for (let i = 0; i < needsSubmit.length; i++) {
    const bottle = needsSubmit[i]
    if (i >= operatorUtxos.length) {
      console.warn(`[deliver] Sem UTxO do operador disponível para garrafa ${bottle.id} (${i + 1}/${needsSubmit.length}). Processe novamente após confirmação.`)
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

  // Idempotência: reusa txs pendentes para `shredded` em vez de duplicar submit
  const pendingByBottle = new Map<string, txsDb.BlockchainTx>()
  const needsSubmit: typeof readyBottles = []
  for (const bottle of readyBottles) {
    const existing = await txsDb.findPendingByBottleAndStage(bottle.id, 'shredded')
    if (existing) {
      pendingByBottle.set(bottle.id, existing)
    } else {
      needsSubmit.push(bottle)
    }
  }

  const operatorUtxos = needsSubmit.length > 0
    ? await cardano.allocateOperatorUtxos(needsSubmit.length, 4_000_000)
    : []
  if (needsSubmit.length > 0 && operatorUtxos.length === 0) {
    throw new Error('Nenhum UTxO do operador disponível para submeter transações')
  }

  const results: { bottleId: string; txHash: string }[] = []
  for (const [bottleId, tx] of pendingByBottle) {
    if (tx.tx_hash) results.push({ bottleId, txHash: tx.tx_hash })
  }

  for (let i = 0; i < needsSubmit.length; i++) {
    const bottle = needsSubmit[i]
    if (i >= operatorUtxos.length) {
      console.warn(`[shred] Sem UTxO do operador disponível para garrafa ${bottle.id} (${i + 1}/${needsSubmit.length}). Processe novamente após confirmação.`)
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
