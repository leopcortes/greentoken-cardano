import { pool } from '../pool'

export interface BlockchainTx {
  id: string
  bottle_id: string
  stage: string
  tx_hash: string | null
  status: 'pending' | 'confirmed' | 'failed'
  datum_json: string | null
  redeemer_json: string | null
  submitted_at: Date
  confirmed_at: Date | null
}

export async function findById(id: string): Promise<BlockchainTx | null> {
  const { rows } = await pool.query('SELECT * FROM blockchain_txs WHERE id = $1', [id])
  return rows[0] ?? null
}

export async function findPending(): Promise<BlockchainTx[]> {
  const { rows } = await pool.query(
    `SELECT * FROM blockchain_txs WHERE status = 'pending' ORDER BY submitted_at`,
  )
  return rows
}

export async function findByBottleId(bottleId: string): Promise<BlockchainTx[]> {
  const { rows } = await pool.query(
    'SELECT * FROM blockchain_txs WHERE bottle_id = $1 ORDER BY submitted_at DESC',
    [bottleId],
  )
  return rows
}

export async function create(data: {
  bottle_id: string
  stage: string
  tx_hash: string
  datum_json?: string
  redeemer_json?: string
}): Promise<BlockchainTx> {
  const { rows } = await pool.query(
    `INSERT INTO blockchain_txs (bottle_id, stage, tx_hash, datum_json, redeemer_json)
     VALUES ($1, $2, $3, $4, $5) RETURNING *`,
    [data.bottle_id, data.stage, data.tx_hash,
     data.datum_json ?? null, data.redeemer_json ?? null],
  )
  return rows[0]
}

export async function confirm(id: string): Promise<void> {
  await pool.query(
    `UPDATE blockchain_txs SET status = 'confirmed', confirmed_at = NOW() WHERE id = $1`,
    [id],
  )
}

export async function fail(id: string): Promise<void> {
  await pool.query(
    `UPDATE blockchain_txs SET status = 'failed' WHERE id = $1`,
    [id],
  )
}
