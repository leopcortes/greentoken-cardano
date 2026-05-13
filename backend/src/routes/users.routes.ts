import { Router, Request, Response } from 'express'
import { promises as fs } from 'fs'
import * as usersDb from '../db/queries/users'
import * as rewardsDb from '../db/queries/rewards'
import { newMnemonic, deriveWallet, fetchBalance } from '../greenwallet/mesh'
import { encryptMnemonic } from '../greenwallet/crypto'
import { paths } from '../config'
import { requireOwner, requireSelfOrOwner } from '../auth/middleware'

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
router.get('/', requireOwner, async (req: Request, res: Response) => {
  try {
    const role = typeof req.query.role === 'string' ? req.query.role : undefined
    const users = await usersDb.list(role)
    res.json(users)
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// GET /users/:id - detalhe de um usuario
router.get('/:id', requireSelfOrOwner('id'), async (req: Request, res: Response) => {
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
router.post('/', requireOwner, async (req: Request, res: Response) => {
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
router.get('/:id/greenwallet', requireSelfOrOwner('id'), async (req: Request, res: Response) => {
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
router.get('/:id/greenwallet/balance', requireSelfOrOwner('id'), async (req: Request, res: Response) => {
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
router.get('/:id/rewards', requireSelfOrOwner('id'), async (req: Request, res: Response) => {
  try {
    const userId = req.params.id as string
    const rewards = await rewardsDb.findByUserId(userId)
    const total = await rewardsDb.totalByUser(userId)
    res.json({ rewards, total_greentoken: total })
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// POST /users/:id/greenwallet/migrate
// Gera greenwallet nova e preenche os campos pending_* do usuario. Retorna a
// mnemonica APENAS NESTA RESPOSTA - depois disso so' via GET /users/:id/greenwallet/seed
// (apos confirmar a migracao).
router.post('/:id/greenwallet/migrate', requireOwner, async (req: Request, res: Response) => {
  try {
    const userId = req.params.id as string
    const existing = await usersDb.findById(userId)
    if (!existing) return res.status(404).json({ error: 'Usuario nao encontrado' })
    if (existing.has_greenwallet) {
      return res.status(409).json({ error: 'Usuario ja possui greenwallet custodiada' })
    }
    if (existing.has_pending_migration) {
      return res.status(409).json({ error: 'Usuario ja tem migracao pendente' })
    }

    const words = newMnemonic()
    const { address, pubkeyHash } = await deriveWallet(words)
    const encrypted_seed = encryptMnemonic(words)

    const updated = await usersDb.initiateMigration(userId, {
      pending_wallet_address: address,
      pending_pubkey_hash: pubkeyHash,
      encrypted_seed,
    })
    if (!updated) {
      return res.status(409).json({ error: 'Falha ao iniciar migracao (estado inconsistente)' })
    }

    res.status(201).json({
      user: updated,
      new_address: address,
      old_address: existing.wallet_address,
      mnemonic: words,
    })
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// POST /users/:id/greenwallet/migrate/confirm
// Promove os campos pending_* a definitivos. Idempotente: chamar duas vezes
// retorna 404 na segunda (nao ha mais pending).
router.post('/:id/greenwallet/migrate/confirm', requireOwner, async (req: Request, res: Response) => {
  try {
    const updated = await usersDb.confirmMigration(req.params.id as string)
    if (!updated) {
      return res.status(404).json({ error: 'Nenhuma migracao pendente para este usuario' })
    }
    res.json({ user: updated })
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// POST /users/:id/greenwallet/migrate/cancel
// Descarta os campos pending_*. A mnemonica gerada e' perdida (nunca foi
// gravada nos campos definitivos).
router.post('/:id/greenwallet/migrate/cancel', requireOwner, async (req: Request, res: Response) => {
  try {
    const updated = await usersDb.cancelMigration(req.params.id as string)
    if (!updated) {
      return res.status(404).json({ error: 'Nenhuma migracao pendente para este usuario' })
    }
    res.json({ user: updated })
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})
