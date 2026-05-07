import { pool } from '../pool'
import type { EncryptedSeed } from '../../greenwallet/crypto'

export interface User {
  id: string
  role: 'recycler' | 'owner'
  name: string
  email: string
  wallet_address: string | null
  pubkey_hash: string | null
  created_at: Date
}

export type UserWithWallet = User & {
  wallet_address: string
  pubkey_hash: string
}

export function requireWallet(user: User | null, userIdForError?: string): UserWithWallet {
  if (!user) {
    throw new Error(`Usuario nao encontrado${userIdForError ? `: ${userIdForError}` : ''}`)
  }
  if (!user.wallet_address || !user.pubkey_hash) {
    throw new Error(`Usuario ${user.id} nao possui greenwallet associada`)
  }
  return user as UserWithWallet
}

const PUBLIC_COLUMNS =
  'id, role, name, email, wallet_address, pubkey_hash, created_at'

export async function findById(id: string): Promise<User | null> {
  const { rows } = await pool.query(
    `SELECT ${PUBLIC_COLUMNS} FROM users WHERE id = $1`,
    [id],
  )
  return rows[0] ?? null
}

export async function findByEmail(email: string): Promise<User | null> {
  const { rows } = await pool.query(
    `SELECT ${PUBLIC_COLUMNS} FROM users WHERE email = $1`,
    [email],
  )
  return rows[0] ?? null
}

export async function findByWallet(walletAddress: string): Promise<User | null> {
  const { rows } = await pool.query(
    `SELECT ${PUBLIC_COLUMNS} FROM users WHERE wallet_address = $1`,
    [walletAddress],
  )
  return rows[0] ?? null
}

export async function create(data: {
  role: 'recycler' | 'owner'
  name: string
  email: string
  wallet_address: string
  pubkey_hash: string
  encrypted_seed: EncryptedSeed
}): Promise<User> {
  const { encrypted_seed: enc } = data
  const { rows } = await pool.query(
    `INSERT INTO users (
       role, name, email, wallet_address, pubkey_hash,
       mnemonic_ciphertext, mnemonic_iv, mnemonic_auth_tag, encryption_key_version
     )
     VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
     RETURNING ${PUBLIC_COLUMNS}`,
    [
      data.role,
      data.name,
      data.email,
      data.wallet_address,
      data.pubkey_hash,
      enc.ciphertext,
      enc.iv,
      enc.authTag,
      enc.version,
    ],
  )
  return rows[0]
}

export async function list(role?: string): Promise<User[]> {
  if (role) {
    const { rows } = await pool.query(
      `SELECT ${PUBLIC_COLUMNS} FROM users WHERE role = $1 ORDER BY created_at`,
      [role],
    )
    return rows
  }
  const { rows } = await pool.query(
    `SELECT ${PUBLIC_COLUMNS} FROM users ORDER BY created_at`,
  )
  return rows
}

export async function getEncryptedSeed(id: string): Promise<EncryptedSeed | null> {
  const { rows } = await pool.query(
    `SELECT mnemonic_ciphertext, mnemonic_iv, mnemonic_auth_tag, encryption_key_version
     FROM users WHERE id = $1`,
    [id],
  )
  const r = rows[0]
  if (!r || !r.mnemonic_ciphertext || !r.mnemonic_iv || !r.mnemonic_auth_tag || r.encryption_key_version == null) {
    return null
  }
  return {
    ciphertext: r.mnemonic_ciphertext,
    iv: r.mnemonic_iv,
    authTag: r.mnemonic_auth_tag,
    version: r.encryption_key_version,
  }
}
