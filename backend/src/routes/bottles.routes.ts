import { Router, Request, Response } from 'express'
import * as bottleService from '../services/bottle.service'
import * as bottlesDb from '../db/queries/bottles'

export const router = Router()

// GET /bottles - lista garrafas (opcional ?user_id=, ?stage=, ?container_id=)
router.get('/', async (req: Request, res: Response) => {
  try {
    const { user_id, stage, container_id, route_id, station_id } = req.query
    let bottles
    if (user_id) {
      bottles = await bottlesDb.findByUserId(user_id as string)
    } else if (stage) {
      bottles = await bottlesDb.findByStage(stage as string)
    } else if (container_id) {
      bottles = await bottlesDb.findByContainerId(container_id as string)
    } else if (route_id) {
      bottles = await bottlesDb.findByRouteId(route_id as string)
    } else if (station_id) {
      bottles = await bottlesDb.findByStationId(station_id as string)
    } else {
      const { rows } = await (await import('../db/pool')).pool.query(`
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
        ORDER BY b.inserted_at DESC
      `)
      bottles = bottlesDb.parseRows(rows)
    }
    res.json(bottles)
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// GET /bottles/next-number - retorna o proximo numero disponivel para garrafa
router.get('/next-number', async (_req: Request, res: Response) => {
  try {
    const nextNum = await bottlesDb.nextNumber()
    const name = `garrafa-${String(nextNum).padStart(4, '0')}`
    res.json({ next_number: nextNum, next_name: name })
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// GET /bottles/:id - detalhe + historico de uma garrafa
router.get('/:id', async (req: Request, res: Response) => {
  try {
    const history = await bottleService.getHistory(req.params.id as string)
    res.json(history)
  } catch (err: any) {
    if (err.message.includes('nao encontrada')) {
      return res.status(404).json({ error: err.message })
    }
    res.status(500).json({ error: err.message })
  }
})

// POST /bottles - cria uma nova garrafa e submete tx de mint
router.post('/', async (req: Request, res: Response) => {
  try {
    const { user_id, container_id, volume_ml } = req.body
    if (!user_id || !container_id || !volume_ml) {
      return res.status(400).json({ error: 'Campos obrigatorios: user_id, container_id, volume_ml' })
    }

    const result = await bottleService.create({
      userId: user_id,
      containerId: container_id,
      volumeMl: Number(volume_ml),
    })

    res.status(201).json({
      bottle: result.bottle,
      tx_hash: result.txHash,
      message: 'Garrafa criada. Aguardando confirmacao on-chain.',
    })
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// GET /bottles/route/:routeId - lista garrafas de uma rota
router.get('/route/:routeId', async (req: Request, res: Response) => {
  try {
    const bottles = await bottlesDb.findByRouteId(req.params.routeId as string)
    res.json(bottles)
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})
