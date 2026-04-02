import { pool } from '../pool'

export interface Truck {
  id: string
  license_plate: string
  status: string
  last_updated: Date
}

export async function list(): Promise<Truck[]> {
  const { rows } = await pool.query('SELECT * FROM trucks ORDER BY license_plate')
  return rows
}

export async function findById(id: string): Promise<Truck | null> {
  const { rows } = await pool.query('SELECT * FROM trucks WHERE id = $1', [id])
  return rows[0] ?? null
}

export async function create(data: {
  license_plate: string
}): Promise<Truck> {
  const { rows } = await pool.query(
    `INSERT INTO trucks (license_plate) VALUES ($1) RETURNING *`,
    [data.license_plate],
  )
  return rows[0]
}

export async function updateStatus(id: string, status: string): Promise<Truck> {
  const { rows } = await pool.query(
    `UPDATE trucks SET status = $1, last_updated = NOW() WHERE id = $2 RETURNING *`,
    [status, id],
  )
  return rows[0]
}
