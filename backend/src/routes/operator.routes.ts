import { Router, Request, Response } from 'express'
import { promises as fs } from 'fs'
import { paths } from '../config'
import { queryUtxosJson } from '../services/cardano.service'
import { requireOwner } from '../auth/middleware'

export const router = Router()

async function readAddr(filePath: string): Promise<string> {
  return (await fs.readFile(filePath, 'utf8')).trim()
}

// GET /operator/balance - saldo e UTxOs da carteira operadora (payment.addr)
router.get('/balance', requireOwner, async (_req: Request, res: Response) => {
  try {
    const address = await readAddr(paths.operatorAddr)
    const utxosRaw = await queryUtxosJson(address)

    const utxos = Object.entries(utxosRaw).map(([txIn, entry]) => {
      const keys = Object.keys(entry.value)
      const adaOnly = keys.length === 1 && keys[0] === 'lovelace'
      return { txIn, lovelace: entry.value.lovelace, ada_only: adaOnly }
    })

    utxos.sort((a, b) => b.lovelace - a.lovelace)

    const totalLovelace = utxos.reduce((s, u) => s + u.lovelace, 0)
    const adaOnlyCount = utxos.filter((u) => u.ada_only).length

    res.json({
      address,
      total_lovelace: totalLovelace,
      ada: (totalLovelace / 1_000_000).toFixed(6),
      utxo_count: utxos.length,
      ada_only_utxo_count: adaOnlyCount,
      utxos,
    })
  } catch (err: any) {
    res.status(502).json({ error: `Falha ao consultar carteira operadora: ${err.message}` })
  }
})
