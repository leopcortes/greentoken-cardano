import { pool } from '../pool'

export interface User {
  id: string
  role: 'recycler' | 'owner'
  name: string
  email: string
  wallet_address: string
  pubkey_hash: string
  created_at: Date
}

export async function findById(id: string): Promise<User | null> {
  const { rows } = await pool.query('SELECT * FROM users WHERE id = $1', [id])
  return rows[0] ?? null
}

export async function findByEmail(email: string): Promise<User | null> {
  const { rows } = await pool.query('SELECT * FROM users WHERE email = $1', [email])
  return rows[0] ?? null
}

export async function findByWallet(walletAddress: string): Promise<User | null> {
  const { rows } = await pool.query('SELECT * FROM users WHERE wallet_address = $1', [walletAddress])
  return rows[0] ?? null
}

export async function create(data: {
  role: 'recycler' | 'owner'
  name: string
  email: string
  wallet_address: string
  pubkey_hash: string
}): Promise<User> {
  const { rows } = await pool.query(
    `INSERT INTO users (role, name, email, wallet_address, pubkey_hash)
     VALUES ($1, $2, $3, $4, $5) RETURNING *`,
    [data.role, data.name, data.email, data.wallet_address, data.pubkey_hash],
  )
  return rows[0]
}

export async function list(role?: string): Promise<User[]> {
  if (role) {
    const { rows } = await pool.query('SELECT * FROM users WHERE role = $1 ORDER BY created_at', [role])
    return rows
  }
  const { rows } = await pool.query('SELECT * FROM users ORDER BY created_at')
  return rows
}
