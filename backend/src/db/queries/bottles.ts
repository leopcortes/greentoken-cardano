import { pool } from '../pool'

export interface Bottle {
  id: string
  user_id: string
  container_id: string | null
  bottle_id_text: string
  bottle_id_hex: string
  current_stage: string
  utxo_hash: string | null
  utxo_index: number | null
  inserted_at: Date
  compacted_at: Date | null
  collected_at: Date | null
  atstation_at: Date | null
  shredded_at: Date | null
}

export async function findById(id: string): Promise<Bottle | null> {
  const { rows } = await pool.query('SELECT * FROM bottles WHERE id = $1', [id])
  return rows[0] ?? null
}

export async function findByUserId(userId: string): Promise<Bottle[]> {
  const { rows } = await pool.query(
    'SELECT * FROM bottles WHERE user_id = $1 ORDER BY inserted_at DESC', [userId],
  )
  return rows
}

export async function findByContainerId(containerId: string): Promise<Bottle[]> {
  const { rows } = await pool.query(
    'SELECT * FROM bottles WHERE container_id = $1 ORDER BY inserted_at DESC', [containerId],
  )
  return rows
}

export async function findByStage(stage: string): Promise<Bottle[]> {
  const { rows } = await pool.query(
    'SELECT * FROM bottles WHERE current_stage = $1 ORDER BY inserted_at DESC', [stage],
  )
  return rows
}

export async function create(data: {
  user_id: string
  container_id?: string
  bottle_id_text: string
  bottle_id_hex: string
}): Promise<Bottle> {
  const { rows } = await pool.query(
    `INSERT INTO bottles (user_id, container_id, bottle_id_text, bottle_id_hex)
     VALUES ($1, $2, $3, $4) RETURNING *`,
    [data.user_id, data.container_id ?? null, data.bottle_id_text, data.bottle_id_hex],
  )
  return rows[0]
}

export async function updateUtxo(id: string, utxoHash: string, utxoIndex: number): Promise<void> {
  await pool.query(
    'UPDATE bottles SET utxo_hash = $2, utxo_index = $3 WHERE id = $1',
    [id, utxoHash, utxoIndex],
  )
}

export async function clearUtxo(id: string): Promise<void> {
  await pool.query('UPDATE bottles SET utxo_hash = NULL, utxo_index = NULL WHERE id = $1', [id])
}

export async function updateStage(id: string, stage: string): Promise<void> {
  const col = `${stage}_at`
  const allowed = ['inserted_at', 'compacted_at', 'collected_at', 'atstation_at', 'shredded_at']
  if (!allowed.includes(col)) throw new Error(`Coluna de timestamp invalida: ${col}`)

  await pool.query(
    `UPDATE bottles SET current_stage = $2, ${col} = NOW() WHERE id = $1`,
    [id, stage],
  )
}
