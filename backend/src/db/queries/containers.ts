import { pool } from '../pool'

export interface Container {
  id: string
  owner_id: string
  name: string
  location_name: string | null
  latitude: number | null
  longitude: number | null
  capacity_liters: number
  current_volume_liters: number
  status: string
  last_updated: Date
}

// node-pg retorna NUMERIC como string; converte para number
function parseRow(row: any): Container {
  return {
    ...row,
    capacity_liters: parseFloat(row.capacity_liters) || 0,
    current_volume_liters: parseFloat(row.current_volume_liters) || 0,
    latitude: row.latitude != null ? parseFloat(row.latitude) : null,
    longitude: row.longitude != null ? parseFloat(row.longitude) : null,
  }
}

export function parseRows(rows: any[]): Container[] {
  return rows.map(parseRow)
}

export async function findById(id: string): Promise<Container | null> {
  const { rows } = await pool.query('SELECT * FROM containers WHERE id = $1', [id])
  return rows[0] ? parseRow(rows[0]) : null
}

export async function findByStatus(status: string): Promise<Container[]> {
  const { rows } = await pool.query(
    'SELECT * FROM containers WHERE status = $1 ORDER BY last_updated', [status],
  )
  return rows.map(parseRow)
}

export async function findByOwnerId(ownerId: string): Promise<Container[]> {
  const { rows } = await pool.query(
    'SELECT * FROM containers WHERE owner_id = $1 ORDER BY name', [ownerId],
  )
  return rows.map(parseRow)
}

export async function create(data: {
  owner_id: string
  name: string
  location_name?: string
  latitude?: number
  longitude?: number
  capacity_liters: number
}): Promise<Container> {
  const { rows } = await pool.query(
    `INSERT INTO containers (owner_id, name, location_name, latitude, longitude, capacity_liters)
     VALUES ($1, $2, $3, $4, $5, $6) RETURNING *`,
    [data.owner_id, data.name, data.location_name ?? null,
     data.latitude ?? null, data.longitude ?? null, data.capacity_liters],
  )
  return parseRow(rows[0])
}

export async function updateVolume(id: string, volumeLiters: number): Promise<Container> {
  const { rows } = await pool.query(
    `UPDATE containers
     SET current_volume_liters = $2, last_updated = NOW()
     WHERE id = $1 RETURNING *`,
    [id, volumeLiters],
  )
  return parseRow(rows[0])
}

export async function updateStatus(id: string, status: string): Promise<void> {
  await pool.query(
    'UPDATE containers SET status = $1, last_updated = NOW() WHERE id = $2',
    [status, id],
  )
}
