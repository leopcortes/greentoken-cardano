import * as txsDb from '../db/queries/blockchain-txs'
import * as bottlesDb from '../db/queries/bottles'
import * as rewardsDb from '../db/queries/rewards'
import { REWARDS_BY_STAGE } from '../db/queries/rewards'
import { isUtxoOnChain, allocateOperatorUtxos } from '../services/cardano.service'
import * as bottleService from '../services/bottle.service'
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

  // Atualiza o estágio da garrafa e o UTxO
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
 * Para cada garrafa recém-validada (stage='inserted' com UTxO confirmado),
 * dispara automaticamente a tx de compactação individual no smart contract.
 * Submete em paralelo com UTxOs do operador pré-alocados (um por garrafa) para
 * evitar contenção. Falhas individuais não abortam o batch - a próxima iteração
 * do worker tentará novamente as garrafas que ficaram para trás.
 */
async function autoCompactPending(): Promise<void> {
  const ready = await bottlesDb.findReadyForAutoCompact()
  if (ready.length === 0) return

  const operatorUtxos = await allocateOperatorUtxos(ready.length, 4_000_000)
  if (operatorUtxos.length === 0) {
    console.warn(`[worker] Sem UTxO do operador disponível - adiando ${ready.length} auto-compactação(ões) para o próximo ciclo.`)
    return
  }

  const batchSize = Math.min(ready.length, operatorUtxos.length)
  if (batchSize < ready.length) {
    console.warn(`[worker] Apenas ${batchSize} UTxO(s) do operador para ${ready.length} garrafa(s). Restante será processado no próximo ciclo.`)
  }

  console.log(`[worker] Auto-compactando ${batchSize} garrafa(s) em paralelo...`)

  const batch = ready.slice(0, batchSize)
  const results = await Promise.allSettled(
    batch.map((bottle, i) =>
      bottleService.autoCompactBottle(bottle.id, operatorUtxos[i].txIn),
    ),
  )

  results.forEach((r, i) => {
    const bottle = batch[i]
    if (r.status === 'fulfilled') {
      if (r.value) {
        console.log(`[worker] Auto-compactação enviada - garrafa ${bottle.bottle_id_text} → tx ${r.value.txHash}`)
      }
    } else {
      console.error(`[worker] Erro ao auto-compactar garrafa ${bottle.bottle_id_text}:`, r.reason)
    }
  })
}

/**
 * Executa um ciclo de polling: busca todas as txs pendentes e processa cada uma.
 */
async function poll(): Promise<void> {
  try {
    const pending = await txsDb.findPending()
    if (pending.length > 0) {
      console.log(`[worker] Verificando ${pending.length} tx(s) pendente(s)...`)
      for (const tx of pending) {
        try {
          await processTransaction(tx)
        } catch (err) {
          console.error(`[worker] Erro ao processar tx ${tx.tx_hash}:`, err)
        }
      }
    }

    // Após confirmar inserções, dispara compactação automática individual
    await autoCompactPending()
  } catch (err) {
    console.error('[worker] Erro no ciclo de polling:', err)
  }
}

/**
 * Inicia o worker de confirmacao com polling periodico.
 *
 * Sem o guard `isRunning`, dois ciclos podem se sobrepor (o poll faz varios
 * cardano-cli, que sao lentos). Quando isso acontece, ambos veem a mesma
 * garrafa em findReadyForAutoCompact e tentam consumir o mesmo UTxO da
 * garrafa, sendo que o segundo falha com "tx input not present in the UTxO".
 */
export function startConfirmationWorker(): NodeJS.Timeout {
  console.log(`[worker] Iniciando polling a cada ${config.CONFIRMATION_POLL_MS}ms`)
  let isRunning = false
  return setInterval(async () => {
    if (isRunning) return
    isRunning = true
    try {
      await poll()
    } finally {
      isRunning = false
    }
  }, config.CONFIRMATION_POLL_MS)
}
