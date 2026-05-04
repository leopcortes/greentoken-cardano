import { Router, Request, Response } from 'express'
import * as stationsDb from '../db/queries/stations'
import * as bottlesDb from '../db/queries/bottles'
import * as bottleService from '../services/bottle.service'

export const router = Router()

// GET /stations - lista estacoes de tratamento
router.get('/', async (_req: Request, res: Response) => {
  try {
    const stations = await stationsDb.list()
    res.json(stations)
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// GET /stations/:id
router.get('/:id', async (req: Request, res: Response) => {
  try {
    const station = await stationsDb.findById(req.params.id as string)
    if (!station) return res.status(404).json({ error: 'Estacao nao encontrada' })
    res.json(station)
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// POST /stations - cria uma nova estacao
router.post('/', async (req: Request, res: Response) => {
  try {
    const { name, location_name, latitude, longitude } = req.body
    if (!name) {
      return res.status(400).json({ error: 'Campo obrigatorio: name' })
    }
    const station = await stationsDb.create({ name, location_name, latitude, longitude })
    res.status(201).json(station)
  } catch (err: any) {
    if (err?.code === '23505') {
      return res.status(409).json({ error: 'Ja existe uma estacao com esse nome neste local' })
    }
    res.status(500).json({ error: err.message })
  }
})

// GET /stations/:id/bottles - lista garrafas na estacao
router.get('/:id/bottles', async (req: Request, res: Response) => {
  try {
    const bottles = await bottlesDb.findByStationId(req.params.id as string)
    res.json(bottles)
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// POST /stations/:id/shred - tritura todas as garrafas atstation da estacao
router.post('/:id/shred', async (req: Request, res: Response) => {
  try {
    const result = await bottleService.shredStation(req.params.id as string)
    res.json({
      message: `${result.shredded} garrafa(s) triturada(s).`,
      ...result,
    })
  } catch (err: any) {
    if (err.message.includes('Nenhuma') || err.message.includes('nao encontrada')) {
      return res.status(400).json({ error: err.message })
    }
    res.status(500).json({ error: err.message })
  }
})
