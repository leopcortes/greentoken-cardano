import { createCipheriv, createDecipheriv, randomBytes, hkdfSync } from 'crypto'
import { config } from '../config'

export interface EncryptedSeed {
  ciphertext: Buffer
  iv: Buffer
  authTag: Buffer
  version: number
}

let _master: Buffer | null = null
let _keyV1: Buffer | null = null

function getMaster(): Buffer {
  if (_master) return _master
  const buf = Buffer.from(config.WALLET_ENCRYPTION_KEY, 'base64')
  if (buf.length !== 32) {
    throw new Error(`WALLET_ENCRYPTION_KEY deve ter 32 bytes em base64; recebido ${buf.length}`)
  }
  _master = buf
  return _master
}

function getKeyV1(): Buffer {
  if (_keyV1) return _keyV1
  const derived = hkdfSync('sha256', getMaster(), Buffer.alloc(0), 'greenwallet-mnemonic-v1', 32)
  _keyV1 = Buffer.from(derived as ArrayBuffer)
  return _keyV1
}

function getKey(version: number): Buffer {
  if (version === 1) return getKeyV1()
  throw new Error(`encryption_key_version desconhecido: ${version}`)
}

export function encryptMnemonic(words: string[]): EncryptedSeed {
  const iv = randomBytes(12)
  const cipher = createCipheriv('aes-256-gcm', getKeyV1(), iv)
  const ct = Buffer.concat([cipher.update(words.join(' '), 'utf8'), cipher.final()])
  return {
    ciphertext: ct,
    iv,
    authTag: cipher.getAuthTag(),
    version: 1,
  }
}

export function decryptMnemonic(enc: EncryptedSeed): string[] {
  const dec = createDecipheriv('aes-256-gcm', getKey(enc.version), enc.iv)
  dec.setAuthTag(enc.authTag)
  const pt = Buffer.concat([dec.update(enc.ciphertext), dec.final()]).toString('utf8')
  return pt.split(' ')
}
