import { Router, Request, Response } from 'express'
import * as trucksDb from '../db/queries/trucks'
import { requireOwner } from '../auth/middleware'

export const router = Router()

router.use(requireOwner)

// GET /trucks - lista caminhoes
router.get('/', async (_req: Request, res: Response) => {
  try {
    const trucks = await trucksDb.list()
    res.json(trucks)
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// GET /trucks/:id
router.get('/:id', async (req: Request, res: Response) => {
  try {
    const truck = await trucksDb.findById(req.params.id as string)
    if (!truck) return res.status(404).json({ error: 'Caminhao não encontrado' })
    res.json(truck)
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// POST /trucks - cria um novo caminhao
router.post('/', async (req: Request, res: Response) => {
  try {
    const { license_plate } = req.body
    if (!license_plate) {
      return res.status(400).json({ error: 'Campo obrigatorio: license_plate' })
    }
    const truck = await trucksDb.create({ license_plate })
    res.status(201).json(truck)
  } catch (err: any) {
    if (err.constraint === 'trucks_license_plate_key') {
      return res.status(409).json({ error: 'Placa ja cadastrada' })
    }
    res.status(500).json({ error: err.message })
  }
})

// PATCH /trucks/:id/status - atualiza status
router.patch('/:id/status', async (req: Request, res: Response) => {
  try {
    const { status } = req.body
    if (!status) {
      return res.status(400).json({ error: 'Campo obrigatorio: status' })
    }
    const truck = await trucksDb.updateStatus(req.params.id as string, status)
    res.json(truck)
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})
