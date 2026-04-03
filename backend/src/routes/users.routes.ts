import { Router, Request, Response } from 'express'
import { execFile } from 'child_process'
import path from 'path'
import * as usersDb from '../db/queries/users'
import * as rewardsDb from '../db/queries/rewards'

export const router = Router()

const PROJECT_ROOT = path.resolve(__dirname, '..', '..', '..')
const GET_PKH_SCRIPT = path.join(PROJECT_ROOT, 'scripts', 'get-pubkey-hash.sh')

function getPubkeyHash(walletAddress: string): Promise<string> {
  return new Promise((resolve, reject) => {
    execFile(GET_PKH_SCRIPT, [walletAddress], (error, stdout, stderr) => {
      if (error) {
        reject(new Error(stderr.trim() || error.message))
        return
      }
      const pkh = stdout.trim()
      if (pkh.length !== 56) {
        reject(new Error(`Pubkey hash invalido extraido do endereco (${pkh.length} chars)`))
        return
      }
      resolve(pkh)
    })
  })
}

// GET /users - lista usuarios (opcional ?role=recycler|owner)
router.get('/', async (req: Request, res: Response) => {
  try {
    const role = typeof req.query.role === 'string' ? req.query.role : undefined
    const users = await usersDb.list(role)
    res.json(users)
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// GET /users/:id - detalhe de um usuario
router.get('/:id', async (req: Request, res: Response) => {
  try {
    const user = await usersDb.findById(req.params.id as string)
    if (!user) return res.status(404).json({ error: 'Usuario nao encontrado' })
    res.json(user)
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// POST /users - cria um novo usuario
router.post('/', async (req: Request, res: Response) => {
  try {
    const { role, name, email, wallet_address } = req.body
    if (!role || !name || !email || !wallet_address) {
      return res.status(400).json({ error: 'Campos obrigatorios: role, name, email, wallet_address' })
    }

    const pubkey_hash = await getPubkeyHash(wallet_address)

    const user = await usersDb.create({ role, name, email, wallet_address, pubkey_hash })
    res.status(201).json(user)
  } catch (err: any) {
    if (err.constraint === 'users_email_key') {
      return res.status(409).json({ error: 'Email ja cadastrado' })
    }
    res.status(500).json({ error: err.message })
  }
})

// GET /users/:id/rewards - historico de recompensas de um usuario
router.get('/:id/rewards', async (req: Request, res: Response) => {
  try {
    const userId = req.params.id as string
    const rewards = await rewardsDb.findByUserId(userId)
    const total = await rewardsDb.totalByUser(userId)
    res.json({ rewards, total_greentoken: total })
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})
