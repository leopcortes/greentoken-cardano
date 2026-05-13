import bcrypt from 'bcrypt'

const COST = 10

export function hashPassword(plain: string): Promise<string> {
  return bcrypt.hash(plain, COST)
}

export function verifyPassword(plain: string, hash: string): Promise<boolean> {
  return bcrypt.compare(plain, hash)
}

export function assertStrongPassword(pw: unknown): asserts pw is string {
  if (typeof pw !== 'string' || pw.length < 8) {
    throw new Error('senha deve ter pelo menos 8 caracteres')
  }
}
