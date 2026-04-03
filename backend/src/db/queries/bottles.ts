import { pool } from '../pool'

export interface Bottle {
  id: string
  user_id: string
  container_id: string | null
  container_name: string | null
  route_id: string | null
  station_id: string | null
  station_name: string | null
  bottle_id_text: string
  bottle_id_hex: string
  volume_ml: number
  current_stage: string
  utxo_hash: string | null
  utxo_index: number | null
  inserted_at: Date
  compacted_at: Date | null
  collected_at: Date | null
  atstation_at: Date | null
  shredded_at: Date | null
}

const BASE_SELECT = `
  SELECT
    b.*,
    c.name AS container_name,
    s.name AS station_name
  FROM bottles b
  LEFT JOIN containers c ON c.id = b.container_id
  LEFT JOIN stations s ON s.id = b.station_id
`

// node-pg retorna NUMERIC como string; converte para number
function parseRow(row: any): Bottle {
  return {
    ...row,
    volume_ml: parseFloat(row.volume_ml) || 0,
    container_name: row.container_name ?? null,
    station_name: row.station_name ?? null,
  }
}

export function parseRows(rows: any[]): Bottle[] {
  return rows.map(parseRow)
}

export async function findAll(): Promise<Bottle[]> {
  const { rows } = await pool.query(
    `${BASE_SELECT} ORDER BY b.inserted_at DESC`,
  )
  return rows.map(parseRow)
}

export async function findById(id: string): Promise<Bottle | null> {
  const { rows } = await pool.query(
    `${BASE_SELECT} WHERE b.id = $1`,
    [id],
  )
  return rows[0] ? parseRow(rows[0]) : null
}

export async function findByUserId(userId: string): Promise<Bottle[]> {
  const { rows } = await pool.query(
    `${BASE_SELECT} WHERE b.user_id = $1 ORDER BY b.inserted_at DESC`,
    [userId],
  )
  return rows.map(parseRow)
}

export async function findByContainerId(containerId: string): Promise<Bottle[]> {
  const { rows } = await pool.query(
    `${BASE_SELECT} WHERE b.container_id = $1 ORDER BY b.inserted_at DESC`,
    [containerId],
  )
  return rows.map(parseRow)
}

export async function findByStage(stage: string): Promise<Bottle[]> {
  const { rows } = await pool.query(
    `${BASE_SELECT} WHERE b.current_stage = $1 ORDER BY b.inserted_at DESC`,
    [stage],
  )
  return rows.map(parseRow)
}

export async function nextNumber(): Promise<number> {
  const { rows } = await pool.query(
    `SELECT COALESCE(MAX(
      CAST(SUBSTRING(bottle_id_text FROM 'garrafa-(\\d+)') AS INTEGER)
    ), 0) AS max_num FROM bottles WHERE bottle_id_text ~ '^garrafa-\\d+$'`,
  )
  return (rows[0]?.max_num ?? 0) + 1
}

export async function create(data: {
  user_id: string
  container_id?: string
  bottle_id_text: string
  bottle_id_hex: string
  volume_ml: number
}): Promise<Bottle> {
  const { rows } = await pool.query(
    `INSERT INTO bottles (user_id, container_id, bottle_id_text, bottle_id_hex, volume_ml)
     VALUES ($1, $2, $3, $4, $5) RETURNING *`,
    [data.user_id, data.container_id ?? null, data.bottle_id_text, data.bottle_id_hex, data.volume_ml],
  )
  return parseRow(rows[0])
}

export async function findByRouteId(routeId: string): Promise<Bottle[]> {
  const { rows } = await pool.query(
    `${BASE_SELECT} WHERE b.route_id = $1 ORDER BY b.inserted_at DESC`,
    [routeId],
  )
  return rows.map(parseRow)
}

export async function findByStationId(stationId: string): Promise<Bottle[]> {
  const { rows } = await pool.query(
    `${BASE_SELECT} WHERE b.station_id = $1 ORDER BY b.inserted_at DESC`,
    [stationId],
  )
  return rows.map(parseRow)
}

export async function findByContainerIdAndStage(containerId: string, stage: string): Promise<Bottle[]> {
  const { rows } = await pool.query(
    `${BASE_SELECT}
     WHERE b.container_id = $1 AND b.current_stage = $2
     ORDER BY b.inserted_at DESC`,
    [containerId, stage],
  )
  return rows.map(parseRow)
}

// Batch: compacta todas as garrafas inserted de um container
export async function compactByContainer(containerId: string): Promise<number> {
  const { rowCount } = await pool.query(
    `UPDATE bottles SET current_stage = 'compacted', compacted_at = NOW()
     WHERE container_id = $1 AND current_stage = 'inserted'`,
    [containerId],
  )
  return rowCount ?? 0
}

// Batch: move garrafas compacted de um container para uma rota (collected)
export async function collectByContainer(containerId: string, routeId: string): Promise<number> {
  const { rowCount } = await pool.query(
    `UPDATE bottles
     SET current_stage = 'collected', collected_at = NOW(),
         container_id = NULL, route_id = $2
     WHERE container_id = $1 AND current_stage = 'compacted'`,
    [containerId, routeId],
  )
  return rowCount ?? 0
}

// Batch: move garrafas collected de uma rota para uma estacao (atstation)
export async function deliverByRoute(routeId: string, stationId: string): Promise<number> {
  const { rowCount } = await pool.query(
    `UPDATE bottles
     SET current_stage = 'atstation', atstation_at = NOW(),
         route_id = NULL, station_id = $2
     WHERE route_id = $1 AND current_stage = 'collected'`,
    [routeId, stationId],
  )
  return rowCount ?? 0
}

// Tritura uma garrafa na estacao
export async function shred(id: string): Promise<void> {
  await pool.query(
    `UPDATE bottles SET current_stage = 'shredded', shredded_at = NOW()
     WHERE id = $1 AND current_stage = 'atstation'`,
    [id],
  )
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
