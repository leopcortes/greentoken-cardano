import { pool } from '../pool'

export interface Bottle {
  id: string
  user_id: string
  container_id: string | null
  container_name: string | null
  route_id: string | null
  station_id: string | null
  station_name: string | null
  truck_license_plate: string | null
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
    s.name AS station_name,
    t.license_plate AS truck_license_plate
  FROM bottles b
  LEFT JOIN containers c ON c.id = b.container_id
  LEFT JOIN stations s ON s.id = b.station_id
  LEFT JOIN routes r ON r.id = b.route_id
  LEFT JOIN trucks t ON t.id = r.truck_id
`

// node-pg retorna NUMERIC como string; converte para number
function parseRow(row: any): Bottle {
  return {
    ...row,
    volume_ml: parseFloat(row.volume_ml) || 0,
    container_name: row.container_name ?? null,
    station_name: row.station_name ?? null,
    truck_license_plate: row.truck_license_plate ?? null,
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

// Garrafas inserted com UTxO confirmado e sem tx de compactação ainda submetida.
// Usado pelo worker para auto-compactar individualmente cada garrafa após validação.
export async function findReadyForAutoCompact(): Promise<Bottle[]> {
  const { rows } = await pool.query(
    `${BASE_SELECT}
     WHERE b.current_stage = 'inserted'
       AND b.utxo_hash IS NOT NULL
       AND b.utxo_index IS NOT NULL
       AND NOT EXISTS (
         SELECT 1 FROM blockchain_txs t
         WHERE t.bottle_id = b.id AND t.stage = 'compacted'
       )
     ORDER BY b.inserted_at ASC`,
  )
  return rows.map(parseRow)
}

// Batch: move garrafas compacted para uma rota por IDs (collected)
export async function collectByIds(bottleIds: string[], routeId: string): Promise<number> {
  if (bottleIds.length === 0) return 0
  const { rowCount } = await pool.query(
    `UPDATE bottles
     SET current_stage = 'collected', collected_at = NOW(),
         container_id = NULL, route_id = $2
     WHERE id = ANY($1) AND current_stage = 'compacted'`,
    [bottleIds, routeId],
  )
  return rowCount ?? 0
}

// Batch: move garrafas collected para uma estação por IDs (atstation)
export async function deliverByIds(bottleIds: string[], stationId: string): Promise<number> {
  if (bottleIds.length === 0) return 0
  const { rowCount } = await pool.query(
    `UPDATE bottles
     SET current_stage = 'atstation', atstation_at = NOW(),
         route_id = NULL, station_id = $2
     WHERE id = ANY($1) AND current_stage = 'collected'`,
    [bottleIds, stationId],
  )
  return rowCount ?? 0
}

// Tritura uma garrafa na estação
export async function shred(id: string): Promise<void> {
  await pool.query(
    `UPDATE bottles SET current_stage = 'shredded', shredded_at = NOW()
     WHERE id = $1 AND current_stage = 'atstation'`,
    [id],
  )
}

// Batch: tritura garrafas atstation por IDs
export async function shredByIds(bottleIds: string[]): Promise<number> {
  if (bottleIds.length === 0) return 0
  const { rowCount } = await pool.query(
    `UPDATE bottles SET current_stage = 'shredded', shredded_at = NOW()
     WHERE id = ANY($1) AND current_stage = 'atstation'`,
    [bottleIds],
  )
  return rowCount ?? 0
}

export async function updateVolume(id: string, volumeMl: number): Promise<void> {
  await pool.query('UPDATE bottles SET volume_ml = $2 WHERE id = $1', [id, volumeMl])
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

// Soma o volume *projetado pos-compactacao* (volume_ml * 0.5 / 1000) das garrafas
// ainda em estagio 'inserted' deste container. Sao garrafas que ja foram
// validadas pela IA mas ainda nao tiveram a tx de compactacao confirmada (e
// portanto current_volume_liters do container ainda nao foi incrementado por
// elas). Necessario para projetar o volume final sem subestimar.
export async function pendingInsertedLiters(containerId: string): Promise<number> {
  const { rows } = await pool.query(
    `SELECT COALESCE(SUM(volume_ml), 0)::float AS total_ml
     FROM bottles
     WHERE container_id = $1 AND current_stage = 'inserted'`,
    [containerId],
  )
  const totalMl = parseFloat(rows[0]?.total_ml ?? '0') || 0
  return (totalMl * 0.5) / 1000
}

export async function deleteById(id: string): Promise<void> {
  // Remove dependencias antes da garrafa (FKs ON DELETE RESTRICT).
  await pool.query('DELETE FROM rewards WHERE bottle_id = $1', [id])
  await pool.query('DELETE FROM blockchain_txs WHERE bottle_id = $1', [id])
  await pool.query('DELETE FROM bottles WHERE id = $1', [id])
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
