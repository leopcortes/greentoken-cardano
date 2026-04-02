import { Router, Request, Response } from 'express'
import * as bottleService from '../services/bottle.service'
import * as bottlesDb from '../db/queries/bottles'

export const router = Router()

// GET /bottles - lista garrafas (opcional ?user_id=, ?stage=, ?container_id=)
router.get('/', async (req: Request, res: Response) => {
  try {
    const { user_id, stage, container_id } = req.query
    let bottles
    if (user_id) {
      bottles = await bottlesDb.findByUserId(user_id as string)
    } else if (stage) {
      bottles = await bottlesDb.findByStage(stage as string)
    } else if (container_id) {
      bottles = await bottlesDb.findByContainerId(container_id as string)
    } else {
      const { rows } = await (await import('../db/pool')).pool.query(
        'SELECT * FROM bottles ORDER BY inserted_at DESC'
      )
      bottles = rows
    }
    res.json(bottles)
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
    const { bottle_id, user_id, container_id } = req.body
    if (!bottle_id || !user_id) {
      return res.status(400).json({ error: 'Campos obrigatorios: bottle_id, user_id' })
    }

    const result = await bottleService.create({
      bottleIdText: bottle_id,
      userId: user_id,
      containerId: container_id,
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

// POST /bottles/:id/advance - avanca a garrafa para o proximo estagio
router.post('/:id/advance', async (req: Request, res: Response) => {
  try {
    const { stage } = req.body
    if (!stage) {
      return res.status(400).json({ error: 'Campo obrigatorio: stage (compacted|collected|atstation|shredded)' })
    }

    const result = await bottleService.advance({
      bottleId: req.params.id as string,
      targetStage: stage as string,
    })

    res.json({
      bottle_id: result.bottle.id,
      tx_hash: result.txHash,
      new_stage: result.targetStage,
      message: 'Transicao submetida. Aguardando confirmacao on-chain.',
    })
  } catch (err: any) {
    if (err.message.includes('nao encontrada') || err.message.includes('nao confirmado')) {
      return res.status(400).json({ error: err.message })
    }
    res.status(500).json({ error: err.message })
  }
})
