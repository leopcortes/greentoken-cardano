import * as bottlesDb from '../db/queries/bottles'
import * as usersDb from '../db/queries/users'
import * as txsDb from '../db/queries/blockchain-txs'
import * as rewardsDb from '../db/queries/rewards'
import * as cardano from './cardano.service'
import { REWARDS_BY_STAGE } from '../db/queries/rewards'
import { validateBottleId, validateStage, validateUUID } from '../validate'

// Mapa de transicoes validas
const TRANSITIONS: Record<string, string> = {
  compacted: 'inserted',
  collected: 'compacted',
  atstation: 'collected',
  shredded: 'atstation',
}

/**
 * Cria uma nova garrafa: gera datums, submete tx de mint e registra no banco.
 * O UTxO sera preenchido pelo confirmation worker apos confirmacao on-chain.
 */
export async function create(params: {
  bottleIdText: string
  userId: string
  containerId?: string
}) {
  const bottleId = validateBottleId(params.bottleIdText)
  const userId = validateUUID(params.userId)

  const user = await usersDb.findById(userId)
  if (!user) throw new Error(`Usuario nao encontrado: ${userId}`)

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
    container_id: params.containerId,
    bottle_id_text: bottleId,
    bottle_id_hex: bottleHex,
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

  return { bottle, txHash }
}

/**
 * Avanca uma garrafa para o proximo estagio: busca UTxO no banco,
 * submete tx de transicao e registra no banco.
 */
export async function advance(params: {
  bottleId: string
  targetStage: string
}) {
  const bottleDbId = validateUUID(params.bottleId)
  const targetStage = validateStage(params.targetStage)

  const sourceStage = TRANSITIONS[targetStage]
  if (!sourceStage) throw new Error(`Transicao invalida para: ${targetStage}`)

  const bottle = await bottlesDb.findById(bottleDbId)
  if (!bottle) throw new Error(`Garrafa nao encontrada: ${bottleDbId}`)

  // Verifica que o estagio atual bate com a origem esperada
  if (bottle.current_stage !== sourceStage) {
    throw new Error(
      `Garrafa esta em '${bottle.current_stage}', esperava '${sourceStage}' para avancar para '${targetStage}'`,
    )
  }

  // UTxO precisa estar disponivel (confirmado pelo worker)
  if (!bottle.utxo_hash || bottle.utxo_index === null) {
    throw new Error('UTxO da garrafa ainda nao confirmado. Aguarde a confirmacao on-chain.')
  }

  const user = await usersDb.findById(bottle.user_id)
  if (!user) throw new Error(`Usuario da garrafa nao encontrado: ${bottle.user_id}`)

  // 1. Submete tx de avanco de estagio
  const txHash = await cardano.advanceStage({
    bottleId: bottle.bottle_id_text,
    targetStage,
    userAddr: user.wallet_address,
    utxoHash: bottle.utxo_hash,
    utxoIndex: bottle.utxo_index,
  })

  // 2. Registra a tx como pending
  const stageNum: Record<string, number> = { inserted: 0, compacted: 1, collected: 2, atstation: 3, shredded: 4 }
  const redeemerJson = JSON.stringify({
    "constructor": 0,
    "fields": [
      { "constructor": stageNum[targetStage], "fields": [] },
    ],
  })

  await txsDb.create({
    bottle_id: bottle.id,
    stage: targetStage,
    tx_hash: txHash,
    redeemer_json: redeemerJson,
  })

  // 3. Limpa o UTxO (sera atualizado apos confirmacao)
  await bottlesDb.clearUtxo(bottle.id)

  return { bottle, txHash, targetStage }
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
