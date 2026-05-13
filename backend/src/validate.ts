const SAFE_ID = /^[a-zA-Z0-9_-]{1,128}$/
const HEX_64 = /^[a-f0-9]{64}$/
const UUID_RE = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i
const BECH32_ADDR = /^addr_?[a-z0-9_]{10,120}$/

export function validateBottleId(id: string): string {
  if (!SAFE_ID.test(id)) throw new Error(`Bottle ID invalido: ${id}`)
  return id
}

export function validateTxHash(hash: string): string {
  if (!HEX_64.test(hash)) throw new Error(`Tx hash invalido: ${hash}`)
  return hash
}

export function validateUUID(id: string): string {
  if (!UUID_RE.test(id)) throw new Error(`UUID invalido: ${id}`)
  return id
}

export function validateAddress(addr: string): string {
  if (!BECH32_ADDR.test(addr)) throw new Error(`Endereço invalido: ${addr}`)
  return addr
}

export function validateStage(stage: string): string {
  const valid = ['inserted', 'compacted', 'collected', 'atstation', 'shredded']
  if (!valid.includes(stage)) throw new Error(`Estágio invalido: ${stage}`)
  return stage
}

export function validateTxIndex(index: number): number {
  if (!Number.isInteger(index) || index < 0) throw new Error(`Tx index invalido: ${index}`)
  return index
}
