import { pool } from '../pool'

export interface Station {
  id: string
  name: string
  location_name: string | null
  latitude: number | null
  longitude: number | null
  created_at: Date
}

function parseRow(row: any): Station {
  return {
    ...row,
    latitude: row.latitude != null ? parseFloat(row.latitude) : null,
    longitude: row.longitude != null ? parseFloat(row.longitude) : null,
  }
}

export function parseRows(rows: any[]): Station[] {
  return rows.map(parseRow)
}

export async function list(): Promise<Station[]> {
  const { rows } = await pool.query('SELECT * FROM stations ORDER BY name')
  return rows.map(parseRow)
}

export async function findById(id: string): Promise<Station | null> {
  const { rows } = await pool.query('SELECT * FROM stations WHERE id = $1', [id])
  return rows[0] ? parseRow(rows[0]) : null
}

export async function create(data: {
  name: string
  location_name?: string
  latitude?: number
  longitude?: number
}): Promise<Station> {
  const { rows } = await pool.query(
    `INSERT INTO stations (name, location_name, latitude, longitude)
     VALUES ($1, $2, $3, $4) RETURNING *`,
    [data.name, data.location_name ?? null, data.latitude ?? null, data.longitude ?? null],
  )
  return parseRow(rows[0])
}
