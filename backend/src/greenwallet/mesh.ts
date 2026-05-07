import {
  generateMnemonic,
  BlockfrostProvider,
  MeshWallet,
  resolvePaymentKeyHash,
} from '@meshsdk/core'
import { validateMnemonic } from 'bip39'
import { config } from '../config'

let _provider: BlockfrostProvider | null = null

function getProvider(): BlockfrostProvider {
  if (!_provider) _provider = new BlockfrostProvider(config.BLOCKFROST_API_KEY)
  return _provider
}

export function newMnemonic(): string[] {
  return generateMnemonic(256).split(' ')
}

export function isValidMnemonic(words: string[]): boolean {
  return words.length === 24 && validateMnemonic(words.join(' '))
}

export async function deriveWallet(words: string[]): Promise<{
  address: string
  pubkeyHash: string
}> {
  if (!isValidMnemonic(words)) {
    throw new Error('Mnemonica invalida (esperado 24 palavras BIP-39 com checksum valido)')
  }
  const provider = getProvider()
  const wallet = new MeshWallet({
    networkId: 0,
    fetcher: provider,
    submitter: provider,
    key: { type: 'mnemonic', words },
  })
  await wallet.init()
  const address = await wallet.getChangeAddress()
  const pubkeyHash = resolvePaymentKeyHash(address)
  return { address, pubkeyHash }
}

export interface AssetBalance {
  unit: string
  quantity: string
}

export async function fetchBalance(address: string): Promise<{
  lovelace: string
  assets: AssetBalance[]
}> {
  const provider = getProvider()
  const utxos = await provider.fetchAddressUTxOs(address)
  let lovelace = 0n
  const assetMap = new Map<string, bigint>()
  for (const u of utxos) {
    for (const a of u.output.amount) {
      const qty = BigInt(a.quantity)
      if (a.unit === 'lovelace') {
        lovelace += qty
      } else {
        assetMap.set(a.unit, (assetMap.get(a.unit) ?? 0n) + qty)
      }
    }
  }
  const assets: AssetBalance[] = Array.from(assetMap.entries()).map(([unit, q]) => ({
    unit,
    quantity: q.toString(),
  }))
  return { lovelace: lovelace.toString(), assets }
}
