import jwt, { JwtPayload } from 'jsonwebtoken'
import { config } from '../config'

export type OwnerTokenPayload = {
  role: 'owner'
}

export type RecyclerTokenPayload = {
  role: 'recycler'
  userId: string
  walletAddress: string
}

export type AuthPayload = OwnerTokenPayload | RecyclerTokenPayload

export function signOwnerToken(): string {
  const payload: OwnerTokenPayload = { role: 'owner' }
  return jwt.sign(payload, config.AUTH_JWT_SECRET, {
    expiresIn: `${config.OWNER_TOKEN_TTL_HOURS}h`,
  })
}

export function signRecyclerToken(userId: string, walletAddress: string): string {
  const payload: RecyclerTokenPayload = { role: 'recycler', userId, walletAddress }
  return jwt.sign(payload, config.AUTH_JWT_SECRET, {
    expiresIn: `${config.RECYCLER_TOKEN_TTL_MIN}m`,
  })
}

export function verifyToken(raw: string): AuthPayload {
  const decoded = jwt.verify(raw, config.AUTH_JWT_SECRET) as JwtPayload & AuthPayload
  if (decoded.role !== 'owner' && decoded.role !== 'recycler') {
    throw new Error('Token com role invalida')
  }
  if (decoded.role === 'recycler' && (!decoded.userId || !decoded.walletAddress)) {
    throw new Error('Token de recycler sem userId/walletAddress')
  }
  return decoded
}
