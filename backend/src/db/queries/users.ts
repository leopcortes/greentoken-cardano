import { pool } from '../pool'
import type { EncryptedSeed } from '../../greenwallet/crypto'

export interface User {
  id: string
  role: 'recycler' | 'owner'
  name: string
  email: string
  wallet_address: string | null
  pubkey_hash: string | null
  has_greenwallet: boolean
  // Migracao pendente: greenwallet nova ja gerada mas ainda não confirmada
  // como definitiva (usuário precisa transferir GTs do endereço antigo).
  pending_wallet_address: string | null
  has_pending_migration: boolean
  migration_initiated_at: Date | null
  created_at: Date
}

export type UserWithWallet = User & {
  wallet_address: string
  pubkey_hash: string
}

export function requireWallet(user: User | null, userIdForError?: string): UserWithWallet {
  if (!user) {
    throw new Error(`Usuário não encontrado${userIdForError ? `: ${userIdForError}` : ''}`)
  }
  if (!user.wallet_address || !user.pubkey_hash) {
    throw new Error(`Usuário ${user.id} não possui greenwallet associada`)
  }
  return user as UserWithWallet
}

// has_greenwallet sinaliza usuarios criados via fluxo greenwallet (mnemonica
// criptografada presente). Usuarios "legados" tem wallet_address informado
// manualmente (Lace) mas sem custódia - has_greenwallet = false.
const PUBLIC_COLUMNS =
  'id, role, name, email, wallet_address, pubkey_hash, ' +
  '(mnemonic_ciphertext IS NOT NULL) AS has_greenwallet, ' +
  'pending_wallet_address, ' +
  '(pending_wallet_address IS NOT NULL) AS has_pending_migration, ' +
  'migration_initiated_at, ' +
  'created_at'

export async function findById(id: string): Promise<User | null> {
  const { rows } = await pool.query(
    `SELECT ${PUBLIC_COLUMNS} FROM users WHERE id = $1`,
    [id],
  )
  return rows[0] ?? null
}

export async function findByEmail(email: string): Promise<User | null> {
  const { rows } = await pool.query(
    `SELECT ${PUBLIC_COLUMNS} FROM users WHERE LOWER(email) = LOWER($1)`,
    [email],
  )
  return rows[0] ?? null
}

// Retorna o hash de senha + dados minimos para o fluxo de login.
// Não usa findByEmail porque PUBLIC_COLUMNS não inclui password_hash.
export async function getAuthRecord(email: string): Promise<{
  id: string
  role: 'recycler' | 'owner'
  password_hash: string | null
  wallet_address: string | null
} | null> {
  const { rows } = await pool.query(
    `SELECT id, role, password_hash, wallet_address
     FROM users WHERE LOWER(email) = LOWER($1)`,
    [email],
  )
  return rows[0] ?? null
}

export async function setPasswordHash(id: string, hash: string): Promise<void> {
  await pool.query(`UPDATE users SET password_hash = $2 WHERE id = $1`, [id, hash])
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
  password_hash?: string | null
  wallet_address?: string | null
  pubkey_hash?: string | null
  encrypted_seed?: EncryptedSeed | null
}): Promise<User> {
  const enc = data.encrypted_seed ?? null
  const { rows } = await pool.query(
    `INSERT INTO users (
       role, name, email, password_hash, wallet_address, pubkey_hash,
       mnemonic_ciphertext, mnemonic_iv, mnemonic_auth_tag, encryption_key_version
     )
     VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
     RETURNING ${PUBLIC_COLUMNS}`,
    [
      data.role,
      data.name,
      data.email,
      data.password_hash ?? null,
      data.wallet_address ?? null,
      data.pubkey_hash ?? null,
      enc?.ciphertext ?? null,
      enc?.iv ?? null,
      enc?.authTag ?? null,
      enc?.version ?? null,
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

// Preenche os campos pending_* com uma greenwallet recem-gerada. Retorna
// o User atualizado. Falha se o usuário ja tem greenwallet custodiada
// (mnemonic_ciphertext IS NOT NULL) ou ja tem migracao pendente.
export async function initiateMigration(
  id: string,
  data: {
    pending_wallet_address: string
    pending_pubkey_hash: string
    encrypted_seed: EncryptedSeed
  },
): Promise<User | null> {
  const { encrypted_seed: enc } = data
  const { rows } = await pool.query(
    `UPDATE users
       SET pending_wallet_address         = $2,
           pending_pubkey_hash            = $3,
           pending_mnemonic_ciphertext    = $4,
           pending_mnemonic_iv            = $5,
           pending_mnemonic_auth_tag      = $6,
           pending_encryption_key_version = $7,
           migration_initiated_at         = NOW()
     WHERE id = $1
       AND mnemonic_ciphertext IS NULL
       AND pending_wallet_address IS NULL
     RETURNING ${PUBLIC_COLUMNS}`,
    [
      id,
      data.pending_wallet_address,
      data.pending_pubkey_hash,
      enc.ciphertext,
      enc.iv,
      enc.authTag,
      enc.version,
    ],
  )
  return rows[0] ?? null
}

// Copia os campos pending_* para os definitivos e limpa os pending_*.
// Retorna o User atualizado, ou null se não havia migracao pendente.
export async function confirmMigration(id: string): Promise<User | null> {
  const { rows } = await pool.query(
    `UPDATE users
       SET wallet_address                 = pending_wallet_address,
           pubkey_hash                    = pending_pubkey_hash,
           mnemonic_ciphertext            = pending_mnemonic_ciphertext,
           mnemonic_iv                    = pending_mnemonic_iv,
           mnemonic_auth_tag              = pending_mnemonic_auth_tag,
           encryption_key_version         = pending_encryption_key_version,
           pending_wallet_address         = NULL,
           pending_pubkey_hash            = NULL,
           pending_mnemonic_ciphertext    = NULL,
           pending_mnemonic_iv            = NULL,
           pending_mnemonic_auth_tag      = NULL,
           pending_encryption_key_version = NULL,
           migration_initiated_at         = NULL
     WHERE id = $1
       AND pending_wallet_address IS NOT NULL
     RETURNING ${PUBLIC_COLUMNS}`,
    [id],
  )
  return rows[0] ?? null
}

// Limpa os campos pending_* sem swap. Retorna o User atualizado, ou null
// se não havia migracao pendente.
export async function cancelMigration(id: string): Promise<User | null> {
  const { rows } = await pool.query(
    `UPDATE users
       SET pending_wallet_address         = NULL,
           pending_pubkey_hash            = NULL,
           pending_mnemonic_ciphertext    = NULL,
           pending_mnemonic_iv            = NULL,
           pending_mnemonic_auth_tag      = NULL,
           pending_encryption_key_version = NULL,
           migration_initiated_at         = NULL
     WHERE id = $1
       AND pending_wallet_address IS NOT NULL
     RETURNING ${PUBLIC_COLUMNS}`,
    [id],
  )
  return rows[0] ?? null
}
