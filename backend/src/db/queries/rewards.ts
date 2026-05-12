import { pool } from '../pool'

export interface Reward {
  id: string
  user_id: string
  bottle_id: string
  bottle_name: string
  tx_id: string | null
  stage: string
  greentoken_amount: number
  tx_hash: string | null
  sent_at: Date
}

// Recompensas por estagio - valores iguais aos dos bash scripts (on-chain)
export const REWARDS_BY_STAGE: Record<string, number> = {
  inserted: 10,
  compacted: 3,
  collected: 7,
  atstation: 10,
  shredded: 20,
}

export async function findByUserId(userId: string): Promise<Reward[]> {
  const { rows } = await pool.query(
    `SELECT
      r.id,
      r.user_id,
      r.bottle_id,
      b.bottle_id_text AS bottle_name,
      r.tx_id,
      r.stage,
      r.greentoken_amount,
      r.tx_hash,
      r.sent_at
    FROM rewards r
    INNER JOIN bottles b ON b.id = r.bottle_id
    WHERE r.user_id = $1
    ORDER BY r.sent_at DESC`, [userId],
  )
  return rows
}

export async function findByBottleId(bottleId: string): Promise<Reward[]> {
  const { rows } = await pool.query(
    'SELECT * FROM rewards WHERE bottle_id = $1 ORDER BY sent_at', [bottleId],
  )
  return rows
}

export async function create(data: {
  user_id: string
  bottle_id: string
  tx_id?: string
  stage: string
  greentoken_amount: number
  tx_hash?: string
}): Promise<Reward | null> {
  const { rows } = await pool.query(
    `INSERT INTO rewards (user_id, bottle_id, tx_id, stage, greentoken_amount, tx_hash)
     VALUES ($1, $2, $3, $4, $5, $6)
     ON CONFLICT (bottle_id, stage) DO NOTHING
     RETURNING *`,
    [data.user_id, data.bottle_id, data.tx_id ?? null,
     data.stage, data.greentoken_amount, data.tx_hash ?? null],
  )
  return rows[0] ?? null
}

export async function totalByUser(userId: string): Promise<number> {
  const { rows } = await pool.query(
    'SELECT COALESCE(SUM(greentoken_amount), 0) AS total FROM rewards WHERE user_id = $1',
    [userId],
  )
  return parseInt(rows[0].total, 10)
}
