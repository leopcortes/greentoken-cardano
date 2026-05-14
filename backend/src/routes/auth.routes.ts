import { Router, Request, Response } from 'express'
import * as usersDb from '../db/queries/users'
import { signOwnerToken, signRecyclerToken } from '../auth/jwt'
import { requireAuth } from '../auth/middleware'
import { hashPassword, verifyPassword, assertStrongPassword } from '../auth/passwords'
import { newMnemonic, isValidMnemonic, deriveWallet } from '../greenwallet/mesh'
import { encryptMnemonic } from '../greenwallet/crypto'

export const router = Router()

type SignupMode = 'new_greenwallet' | 'restore_greenwallet' | 'external_wallet'

function isAddrLike(addr: string): boolean {
  return /^addr(_test)?1[0-9a-z]{20,}$/.test(addr)
}

// POST /auth/login - autentica via email + senha (qualquer role)
router.post('/login', async (req: Request, res: Response) => {
  try {
    const { email, password } = req.body
    if (typeof email !== 'string' || typeof password !== 'string') {
      return res.status(400).json({ error: 'Campos obrigatórios: email, password' })
    }
    const record = await usersDb.getAuthRecord(email)
    if (!record || !record.password_hash) {
      return res.status(401).json({ error: 'Credenciais inválidas' })
    }
    const ok = await verifyPassword(password, record.password_hash)
    if (!ok) {
      return res.status(401).json({ error: 'Credenciais inválidas' })
    }

    const user = await usersDb.findById(record.id)
    if (!user) {
      return res.status(500).json({ error: 'Inconsistência: usuário não encontrado após auth' })
    }

    const token =
      record.role === 'owner'
        ? signOwnerToken(record.id)
        : signRecyclerToken(record.id, record.wallet_address ?? '')
    res.json({ token, user })
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// POST /auth/signup - cadastro publico, apenas para role=recycler.
// 3 modos:
//   - new_greenwallet: gera 24 palavras + deriva wallet + cifra. Retorna palavras 1x.
//   - restore_greenwallet: usuário passa 24 palavras pre-existentes. Re-deriva
//     o mesmo wallet_address (portabilidade entre bancos/PCs).
//   - external_wallet: usuário informa um wallet_address externo (ex: Lace).
router.post('/signup', async (req: Request, res: Response) => {
  try {
    const { name, email, password, mode, mnemonic, wallet_address } = req.body

    if (typeof name !== 'string' || !name.trim()) {
      return res.status(400).json({ error: 'Campo obrigatório: name' })
    }
    if (typeof email !== 'string' || !email.includes('@')) {
      return res.status(400).json({ error: 'Email inválido' })
    }
    try {
      assertStrongPassword(password)
    } catch (e: any) {
      return res.status(400).json({ error: e.message })
    }

    const validModes: SignupMode[] = ['new_greenwallet', 'restore_greenwallet', 'external_wallet']
    if (!validModes.includes(mode)) {
      return res.status(400).json({ error: `mode deve ser um de: ${validModes.join(', ')}` })
    }

    if (await usersDb.findByEmail(email)) {
      return res.status(409).json({ error: 'Email já cadastrado' })
    }

    let walletAddr: string
    let pubkeyHash: string | null = null
    let encryptedSeed: ReturnType<typeof encryptMnemonic> | null = null
    let mnemonicToReturn: string[] | null = null

    if (mode === 'new_greenwallet') {
      const words = newMnemonic()
      const derived = await deriveWallet(words)
      walletAddr = derived.address
      pubkeyHash = derived.pubkeyHash
      encryptedSeed = encryptMnemonic(words)
      mnemonicToReturn = words
    } else if (mode === 'restore_greenwallet') {
      if (!Array.isArray(mnemonic) || mnemonic.length !== 24) {
        return res.status(400).json({ error: 'mnemonic deve ser array de 24 palavras' })
      }
      const words = mnemonic.map((w: unknown) => String(w).trim().toLowerCase())
      if (!isValidMnemonic(words)) {
        return res.status(400).json({ error: 'mnemonica BIP-39 inválida' })
      }
      const derived = await deriveWallet(words)
      walletAddr = derived.address
      pubkeyHash = derived.pubkeyHash
      encryptedSeed = encryptMnemonic(words)
    } else {
      if (typeof wallet_address !== 'string' || !isAddrLike(wallet_address)) {
        return res.status(400).json({ error: 'Endereço da wallet inválido (esperado addr_test1... ou addr1...)' })
      }
      const inUse = await usersDb.findByWallet(wallet_address)
      if (inUse) {
        return res.status(409).json({ error: 'Endereço da wallet já cadastrado para outro usuário' })
      }
      walletAddr = wallet_address
    }

    if (mode !== 'external_wallet') {
      const inUse = await usersDb.findByWallet(walletAddr)
      if (inUse) {
        return res.status(409).json({
          error:
            'Esta greenwallet já esta vinculada a outro usuário neste banco. ' +
            'Faça login com o email correspondente.',
        })
      }
    }

    const password_hash = await hashPassword(password)

    const user = await usersDb.create({
      role: 'recycler',
      name: name.trim(),
      email,
      password_hash,
      wallet_address: walletAddr,
      pubkey_hash: pubkeyHash,
      encrypted_seed: encryptedSeed,
    })

    const token = signRecyclerToken(user.id, walletAddr)
    res.status(201).json({
      token,
      user,
      ...(mnemonicToReturn ? { mnemonic: mnemonicToReturn } : {}),
    })
  } catch (err: any) {
    if (err.constraint === 'users_email_key') {
      return res.status(409).json({ error: 'Email já cadastrado' })
    }
    res.status(500).json({ error: err.message })
  }
})

// GET /auth/me - retorna o payload do token + dados do usuário do DB.
// O frontend usa essa rota para reidratar a sessao apos refresh.
router.get('/me', requireAuth, async (req: Request, res: Response) => {
  try {
    const payload = req.user!
    const user = await usersDb.findById(payload.userId)
    if (!user) return res.status(404).json({ error: 'Usuário não encontrado' })
    res.json({ user, role: payload.role })
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// GET /auth/recyclers - lista publica e' minima de recicladores para o "modo demo"
// do terminal (selecionar da lista em vez de digitar o endereço). Retorna apenas
// id, name e wallet_address - sem email, saldos ou outros dados sensiveis.
// Em producao seria removida em favor de scan de QR code.
router.get('/recyclers', async (_req: Request, res: Response) => {
  try {
    const all = await usersDb.list('recycler')
    res.json(
      all
        .filter((u) => !!u.wallet_address)
        .map((u) => ({ id: u.id, name: u.name, wallet_address: u.wallet_address })),
    )
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})
