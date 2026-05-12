import { Router, Request, Response } from 'express'
import { timingSafeEqual } from 'crypto'
import { config } from '../config'
import * as usersDb from '../db/queries/users'
import { signOwnerToken, signRecyclerToken } from '../auth/jwt'
import { requireAuth } from '../auth/middleware'

export const router = Router()

function safeEqual(a: string, b: string): boolean {
  const bufA = Buffer.from(a, 'utf8')
  const bufB = Buffer.from(b, 'utf8')
  if (bufA.length !== bufB.length) return false
  return timingSafeEqual(bufA, bufB)
}

// POST /auth/owner - autentica o owner via senha unica (OWNER_PASSWORD)
router.post('/owner', async (req: Request, res: Response) => {
  try {
    const { password } = req.body
    if (typeof password !== 'string' || password.length === 0) {
      return res.status(400).json({ error: 'Campo obrigatorio: password' })
    }
    if (!safeEqual(password, config.OWNER_PASSWORD)) {
      return res.status(401).json({ error: 'Senha incorreta' })
    }
    const token = signOwnerToken()
    res.json({ token, role: 'owner' })
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// POST /auth/recycler - "login" do reciclador no kiosk informando o wallet_address.
// Simula o futuro QR + assinatura: o front identifica o usuario pelo endereco da
// greenwallet. Nao ha senha; em producao isso seria substituido por uma prova
// criptografica (CIP-30) de posse da chave privada.
router.post('/recycler', async (req: Request, res: Response) => {
  try {
    const { wallet_address } = req.body
    if (typeof wallet_address !== 'string' || wallet_address.length === 0) {
      return res.status(400).json({ error: 'Campo obrigatorio: wallet_address' })
    }
    const user = await usersDb.findByWallet(wallet_address)
    if (!user) {
      return res.status(404).json({ error: 'Carteira nao encontrada' })
    }
    if (user.role !== 'recycler') {
      return res.status(403).json({ error: 'Apenas recicladores podem entrar pelo kiosk' })
    }
    if (!user.wallet_address) {
      return res.status(400).json({ error: 'Usuario sem wallet_address associado' })
    }
    const token = signRecyclerToken(user.id, user.wallet_address)
    res.json({ token, user })
  } catch (err: any) {
    res.status(500).json({ error: err.message })
  }
})

// GET /auth/me - retorna o payload do token (util para o frontend reidratar sessao)
router.get('/me', requireAuth, async (req: Request, res: Response) => {
  res.json({ user: req.user })
})

// GET /auth/recyclers - lista publica e' minima de recicladores para o "modo demo"
// do kiosk (selecionar da lista em vez de digitar o endereco). Retorna apenas
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
