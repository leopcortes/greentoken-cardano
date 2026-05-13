import { Request, Response, NextFunction } from 'express'
import { AuthPayload, verifyToken } from './jwt'

declare global {
  namespace Express {
    interface Request {
      user?: AuthPayload
    }
  }
}

function extractBearer(req: Request): string | null {
  const header = req.headers.authorization
  if (!header || !header.startsWith('Bearer ')) return null
  return header.slice(7).trim() || null
}

export function requireAuth(req: Request, res: Response, next: NextFunction) {
  const token = extractBearer(req)
  if (!token) {
    return res.status(401).json({ error: 'Token ausente' })
  }
  try {
    req.user = verifyToken(token)
    next()
  } catch (err: any) {
    return res.status(401).json({ error: 'Token invalido ou expirado' })
  }
}

export function requireOwner(req: Request, res: Response, next: NextFunction) {
  const token = extractBearer(req)
  if (!token) {
    return res.status(401).json({ error: 'Token ausente' })
  }
  try {
    const payload = verifyToken(token)
    if (payload.role !== 'owner') {
      return res.status(403).json({ error: 'Acesso restrito ao owner' })
    }
    req.user = payload
    next()
  } catch {
    return res.status(401).json({ error: 'Token invalido ou expirado' })
  }
}

// Permite acesso se o usuário logado for owner OU se for o proprio recycler
// (req.user.userId === req.params[paramName]).
export function requireSelfOrOwner(paramName: string = 'id') {
  return (req: Request, res: Response, next: NextFunction) => {
    const token = extractBearer(req)
    if (!token) {
      return res.status(401).json({ error: 'Token ausente' })
    }
    try {
      const payload = verifyToken(token)
      req.user = payload
      if (payload.role === 'owner') return next()
      const targetId = req.params[paramName]
      if (payload.userId === targetId) return next()
      return res.status(403).json({ error: 'Acesso negado a recursos de outro usuário' })
    } catch {
      return res.status(401).json({ error: 'Token invalido ou expirado' })
    }
  }
}
