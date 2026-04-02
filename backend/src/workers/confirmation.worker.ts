import * as txsDb from '../db/queries/blockchain-txs'
import * as bottlesDb from '../db/queries/bottles'
import * as rewardsDb from '../db/queries/rewards'
import { REWARDS_BY_STAGE } from '../db/queries/rewards'
import { isUtxoOnChain } from '../services/cardano.service'
import { config } from '../config'

// Indice do output do script na transacao (sempre o primeiro --tx-out)
const SCRIPT_OUTPUT_INDEX = 0

/**
 * Processa uma unica transacao pendente:
 * 1. Verifica se o UTxO gerado pela tx existe on-chain
 * 2. Se confirmado: atualiza blockchain_txs, bottles e rewards
 */
async function processTransaction(tx: txsDb.BlockchainTx): Promise<void> {
  if (!tx.tx_hash) return

  const confirmed = await isUtxoOnChain(tx.tx_hash, SCRIPT_OUTPUT_INDEX)
  if (!confirmed) return

  // Marca tx como confirmada
  await txsDb.confirm(tx.id)

  // Atualiza o estagio da garrafa e o UTxO
  await bottlesDb.updateStage(tx.bottle_id, tx.stage)
  await bottlesDb.updateUtxo(tx.bottle_id, tx.tx_hash, SCRIPT_OUTPUT_INDEX)

  // Registra a recompensa
  const bottle = await bottlesDb.findById(tx.bottle_id)
  if (!bottle) return

  const amount = REWARDS_BY_STAGE[tx.stage]
  if (amount) {
    await rewardsDb.create({
      user_id: bottle.user_id,
      bottle_id: bottle.id,
      tx_id: tx.id,
      stage: tx.stage,
      greentoken_amount: amount,
      tx_hash: tx.tx_hash,
    })
  }

  console.log(`[worker] Tx ${tx.tx_hash} confirmada - garrafa ${bottle.bottle_id_text} → ${tx.stage}`)
}

/**
 * Executa um ciclo de polling: busca todas as txs pendentes e processa cada uma.
 */
async function poll(): Promise<void> {
  try {
    const pending = await txsDb.findPending()
    if (pending.length === 0) return

    console.log(`[worker] Verificando ${pending.length} tx(s) pendente(s)...`)

    for (const tx of pending) {
      try {
        await processTransaction(tx)
      } catch (err) {
        console.error(`[worker] Erro ao processar tx ${tx.tx_hash}:`, err)
      }
    }
  } catch (err) {
    console.error('[worker] Erro no ciclo de polling:', err)
  }
}

/**
 * Inicia o worker de confirmacao com polling periodico.
 */
export function startConfirmationWorker(): NodeJS.Timeout {
  console.log(`[worker] Iniciando polling a cada ${config.CONFIRMATION_POLL_MS}ms`)
  return setInterval(poll, config.CONFIRMATION_POLL_MS)
}
