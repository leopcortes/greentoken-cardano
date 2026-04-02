import { Router, Request, Response } from 'express'
import * as routesDb from '../db/queries/routes'

export const router = Router()

// GET /routes - lista rotas com detalhes
router.get('/', async (_req: Request, res: Response) => {
  try {
    const routes = await routesDb.list()
    res.json(routes)
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// GET /routes/:id - detalhe de uma rota
router.get('/:id', async (req: Request, res: Response) => {
  try {
    const route = await routesDb.findById(req.params.id as string)
    if (!route) return res.status(404).json({ error: 'Rota nao encontrada' })
    const stops = await routesDb.findStops(req.params.id as string)
    res.json({ ...route, stops })
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// POST /routes - cria uma nova rota
router.post('/', async (req: Request, res: Response) => {
  try {
    const { truck_id, container_ids } = req.body
    if (!truck_id || !container_ids || !Array.isArray(container_ids) || container_ids.length === 0) {
      return res.status(400).json({ error: 'Campos obrigatorios: truck_id, container_ids (array)' })
    }
    const route = await routesDb.create({ truck_id, container_ids })
    res.status(201).json(route)
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// POST /routes/stops/:stopId/collect - marca parada como coletada
router.post('/stops/:stopId/collect', async (req: Request, res: Response) => {
  try {
    await routesDb.completeStop(req.params.stopId as string)
    res.json({ message: 'Parada coletada com sucesso' })
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})
