import { Router, Request, Response } from 'express'
import * as containersDb from '../db/queries/containers'
import * as containerService from '../services/container.service'

export const router = Router()

// GET /containers - lista containers (opcional ?status=, ?owner_id=)
router.get('/', async (req: Request, res: Response) => {
  try {
    const status = req.query.status as string | undefined
    const ownerId = req.query.owner_id as string | undefined
    let containers
    if (status === 'all') {
      const { pool } = await import('../db/pool')
      const { rows } = await pool.query('SELECT * FROM containers ORDER BY last_updated DESC')
      containers = containersDb.parseRows(rows)
    } else if (status) {
      containers = await containersDb.findByStatus(status)
    } else if (ownerId) {
      containers = await containersDb.findByOwnerId(ownerId)
    } else {
      containers = await containersDb.findByStatus('active')
    }
    res.json(containers)
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// GET /containers/:id - detalhe de um container
router.get('/:id', async (req: Request, res: Response) => {
  try {
    const container = await containersDb.findById(req.params.id as string)
    if (!container) return res.status(404).json({ error: 'Container nao encontrado' })
    res.json(container)
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// POST /containers - cria um novo container
router.post('/', async (req: Request, res: Response) => {
  try {
    const { owner_id, name, location_name, latitude, longitude, capacity_liters } = req.body
    if (!owner_id || !name || !capacity_liters) {
      return res.status(400).json({ error: 'Campos obrigatorios: owner_id, name, capacity_liters' })
    }
    const container = await containersDb.create({
      owner_id, name, location_name, latitude, longitude, capacity_liters,
    })
    res.status(201).json(container)
  } catch (err: any) {
    if (err?.code === '23505') {
      return res.status(409).json({ error: 'Ja existe um container com esse nome para este owner' })
    }
    res.status(500).json({ error: err.message })
  }
})

// POST /containers/:id/deposit - registra volume depositado
router.post('/:id/deposit', async (req: Request, res: Response) => {
  try {
    const { liters } = req.body
    if (!liters || liters <= 0) {
      return res.status(400).json({ error: 'Campo obrigatorio: liters (> 0)' })
    }
    const updated = await containerService.addVolume(req.params.id as string, liters)
    res.json(updated)
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// POST /containers/:id/collected - marca container como coletado (esvaziado)
router.post('/:id/collected', async (req: Request, res: Response) => {
  try {
    await containerService.markCollected(req.params.id as string)
    res.json({ message: 'Container esvaziado e reativado' })
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// POST /containers/:id/compact - compacta garrafas inserted do container (>= 90% cheio)
router.post('/:id/compact', async (req: Request, res: Response) => {
  try {
    const result = await containerService.compact(req.params.id as string)
    res.json({
      message: `${result.compacted} garrafa(s) compactada(s)`,
      ...result,
    })
  } catch (err: any) {
    if (err.message.includes('90%') || err.message.includes('Nenhuma')) {
      return res.status(400).json({ error: err.message })
    }
    res.status(500).json({ error: err.message })
  }
})

// GET /containers/full - lista containers cheios (prontos para rota)
router.get('/status/full', async (_req: Request, res: Response) => {
  try {
    const containers = await containerService.listFull()
    res.json(containers)
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})
