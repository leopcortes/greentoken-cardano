import { Router, Request, Response } from 'express'
import * as routesDb from '../db/queries/routes'
import * as bottleService from '../services/bottle.service'
import { requireOwner } from '../auth/middleware'

export const router = Router()

router.use(requireOwner)

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
    if (!route) return res.status(404).json({ error: 'Rota não encontrada' })
    const stops = await routesDb.findStops(req.params.id as string)
    res.json({ ...route, stops })
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// POST /routes - cria uma nova rota (agora aceita station_id como destino)
router.post('/', async (req: Request, res: Response) => {
  try {
    const { truck_id, station_id, container_ids } = req.body
    if (!truck_id || !container_ids || !Array.isArray(container_ids) || container_ids.length === 0) {
      return res.status(400).json({ error: 'Campos obrigatorios: truck_id, container_ids (array)' })
    }
    const route = await routesDb.create({ truck_id, station_id, container_ids })
    res.status(201).json(route)
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// POST /routes/stops/:stopId/collect - coleta parada: move garrafas compacted -> collected
router.post('/stops/:stopId/collect', async (req: Request, res: Response) => {
  try {
    // 1. Marca parada como coletada no banco (atualiza rota/caminhao se necessario)
    const { routeId, containerId } = await routesDb.completeStop(req.params.stopId as string)

    // 2. Move garrafas compacted do container para a rota
    const result = await bottleService.collectContainer(containerId, routeId)

    res.json({
      message: `Parada coletada. ${result.collected} garrafa(s) movida(s) para a rota.`,
      ...result,
    })
  } catch (err: any) {
    if (
      err.message.includes('aguardando compactação') ||
      err.message.includes('Nenhuma') ||
      err.message.includes('aguardando confirmação')
    ) {
      return res.status(400).json({ error: err.message })
    }
    res.status(500).json({ error: err.message })
  }
})

// POST /routes/:id/deliver - entrega garrafas da rota em uma estação
router.post('/:id/deliver', async (req: Request, res: Response) => {
  try {
    const { station_id } = req.body
    if (!station_id) {
      return res.status(400).json({ error: 'Campo obrigatorio: station_id' })
    }

    // Atualiza station_id na rota se ainda não estava definido
    await routesDb.updateStationId(req.params.id as string, station_id)

    const result = await bottleService.deliverToStation(req.params.id as string, station_id)

    // Mark route as completed and free truck
    await routesDb.completeRoute(req.params.id as string)

    res.json({
      message: `${result.delivered} garrafa(s) entregue(s) na estação. Rota concluida.`,
      ...result,
    })
  } catch (err: any) {
    if (err.message.includes('Nenhuma') || err.message.includes('aguardando confirmação')) {
      return res.status(400).json({ error: err.message })
    }
    res.status(500).json({ error: err.message })
  }
})
