import { Router, Request, Response } from 'express'
import { promises as fs } from 'fs'
import * as usersDb from '../db/queries/users'
import * as rewardsDb from '../db/queries/rewards'
import { newMnemonic, deriveWallet, fetchBalance } from '../greenwallet/mesh'
import { encryptMnemonic } from '../greenwallet/crypto'
import { paths } from '../config'

export const router = Router()

const GREENTOKEN_NAME_HEX = Buffer.from('Greentoken').toString('hex')

let _greentokenUnit: string | null = null
async function greentokenAssetUnit(): Promise<string> {
  if (_greentokenUnit) return _greentokenUnit
  const policyId = (await fs.readFile(paths.policyIdFile, 'utf8')).trim()
  _greentokenUnit = policyId + GREENTOKEN_NAME_HEX
  return _greentokenUnit
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

// POST /users - cria um novo usuario com greenwallet auto-gerada.
// A mnemonica e retornada APENAS NESTA RESPOSTA - nunca mais sera exposta
// sem reautenticacao. Frontend deve forcar o usuario a anotar a frase.
router.post('/', async (req: Request, res: Response) => {
  try {
    const { role, name, email } = req.body
    if (!role || !name || !email) {
      return res.status(400).json({ error: 'Campos obrigatorios: role, name, email' })
    }
    if (role !== 'recycler' && role !== 'owner') {
      return res.status(400).json({ error: "role deve ser 'recycler' ou 'owner'" })
    }

    const words = newMnemonic()
    const { address, pubkeyHash } = await deriveWallet(words)
    const encrypted_seed = encryptMnemonic(words)

    const user = await usersDb.create({
      role,
      name,
      email,
      wallet_address: address,
      pubkey_hash: pubkeyHash,
      encrypted_seed,
    })

    res.status(201).json({ ...user, mnemonic: words })
  } catch (err: any) {
    if (err.constraint === 'users_email_key') {
      return res.status(409).json({ error: 'Email ja cadastrado' })
    }
    res.status(500).json({ error: err.message })
  }
})

// GET /users/:id/greenwallet - dados publicos da greenwallet (sem seed)
router.get('/:id/greenwallet', async (req: Request, res: Response) => {
  try {
    const user = await usersDb.findById(req.params.id as string)
    if (!user) return res.status(404).json({ error: 'Usuario nao encontrado' })
    if (!user.wallet_address || !user.pubkey_hash) {
      return res.status(404).json({ error: 'Usuario nao possui greenwallet associada' })
    }
    res.json({ address: user.wallet_address, pubkey_hash: user.pubkey_hash })
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// GET /users/:id/greenwallet/balance - saldo on-chain (Blockfrost) por endereco
router.get('/:id/greenwallet/balance', async (req: Request, res: Response) => {
  try {
    const user = await usersDb.findById(req.params.id as string)
    if (!user) return res.status(404).json({ error: 'Usuario nao encontrado' })
    if (!user.wallet_address) {
      return res.status(404).json({ error: 'Usuario nao possui endereco Cardano' })
    }
    const { lovelace, assets } = await fetchBalance(user.wallet_address)
    const gtUnit = await greentokenAssetUnit()
    const greentokenAsset = assets.find((a) => a.unit === gtUnit)
    res.json({
      address: user.wallet_address,
      lovelace,
      ada: (Number(lovelace) / 1_000_000).toFixed(6),
      greentoken: greentokenAsset ? Number(greentokenAsset.quantity) : 0,
      assets,
    })
  } catch (err: any) {
    res.status(502).json({ error: `Falha ao consultar saldo on-chain: ${err.message}` })
  }
})

// GET /users/:id/rewards - historico de recompensas de um usuario (DB)
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
