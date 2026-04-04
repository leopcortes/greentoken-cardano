import { pool } from '../pool'

export interface Route {
  id: string
  truck_id: string
  station_id: string | null
  status: string
  created_at: Date
  completed_at: Date | null
}

export interface RouteStop {
  id: string
  route_id: string
  container_id: string
  stop_order: number
  status: string
  collected_at: Date | null
}

export interface RouteWithDetails extends Route {
  truck_license_plate: string
  stop_count: number
}

export async function list(): Promise<RouteWithDetails[]> {
  const { rows } = await pool.query(`
    SELECT r.*, t.license_plate AS truck_license_plate,
           (SELECT COUNT(*) FROM route_stops rs WHERE rs.route_id = r.id)::int AS stop_count
    FROM routes r
    JOIN trucks t ON t.id = r.truck_id
    ORDER BY r.created_at DESC
  `)
  return rows
}

export async function findById(id: string): Promise<Route | null> {
  const { rows } = await pool.query('SELECT * FROM routes WHERE id = $1', [id])
  return rows[0] ?? null
}

export async function findStops(routeId: string): Promise<(RouteStop & { container_name: string })[]> {
  const { rows } = await pool.query(`
    SELECT rs.*, c.name AS container_name
    FROM route_stops rs
    JOIN containers c ON c.id = rs.container_id
    WHERE rs.route_id = $1
    ORDER BY rs.stop_order
  `, [routeId])
  return rows
}

export async function create(data: {
  truck_id: string
  station_id?: string
  container_ids: string[]
}): Promise<Route> {
  const client = await pool.connect()
  try {
    await client.query('BEGIN')

    const { rows: [route] } = await client.query(
      `INSERT INTO routes (truck_id, station_id) VALUES ($1, $2) RETURNING *`,
      [data.truck_id, data.station_id ?? null],
    )

    for (let i = 0; i < data.container_ids.length; i++) {
      await client.query(
        `INSERT INTO route_stops (route_id, container_id, stop_order)
         VALUES ($1, $2, $3)`,
        [route.id, data.container_ids[i], i + 1],
      )
    }

    // Mark truck as on_route
    await client.query(
      `UPDATE trucks SET status = 'on_route', last_updated = NOW() WHERE id = $1`,
      [data.truck_id],
    )

    // Mark containers as in_route
    for (const cid of data.container_ids) {
      await client.query(
        `UPDATE containers SET status = 'in_route', last_updated = NOW() WHERE id = $1`,
        [cid],
      )
    }

    await client.query('COMMIT')
    return route
  } catch (err) {
    await client.query('ROLLBACK')
    throw err
  } finally {
    client.release()
  }
}

// Marca uma parada como coletada (apenas banco - garrafas sao movidas pelo service)
export async function completeStop(stopId: string): Promise<{ routeId: string; containerId: string }> {
  const client = await pool.connect()
  try {
    await client.query('BEGIN')

    const { rows: [stop] } = await client.query(
      `UPDATE route_stops SET status = 'collected', collected_at = NOW()
       WHERE id = $1 RETURNING *`,
      [stopId],
    )

    if (!stop) throw new Error('Parada nao encontrada')

    // Check if all stops are collected -> complete route and free truck
    const { rows: [{ pending }] } = await client.query(
      `SELECT COUNT(*) AS pending FROM route_stops
       WHERE route_id = $1 AND status = 'pending'`,
      [stop.route_id],
    )

    if (parseInt(pending) === 0) {
      await client.query(
        `UPDATE routes SET status = 'completed', completed_at = NOW() WHERE id = $1`,
        [stop.route_id],
      )
      const { rows: [route] } = await client.query(
        `SELECT truck_id FROM routes WHERE id = $1`, [stop.route_id],
      )
      await client.query(
        `UPDATE trucks SET status = 'available', last_updated = NOW() WHERE id = $1`,
        [route.truck_id],
      )
    }

    await client.query('COMMIT')
    return { routeId: stop.route_id, containerId: stop.container_id }
  } catch (err) {
    await client.query('ROLLBACK')
    throw err
  } finally {
    client.release()
  }
}

export async function updateStationId(routeId: string, stationId: string): Promise<void> {
  await pool.query(
    'UPDATE routes SET station_id = $2 WHERE id = $1',
    [routeId, stationId],
  )
}
