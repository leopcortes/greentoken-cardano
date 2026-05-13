import { execFile as execFileCb } from 'child_process'
import { promisify } from 'util'
import * as fs from 'fs/promises'
import * as path from 'path'
import * as os from 'os'
import * as crypto from 'crypto'
import { config, paths } from '../config'
import { REWARDS_BY_STAGE } from '../db/queries/rewards'

const execFile = promisify(execFileCb)

// Mapa de transicoes validas: destino → origem
const TRANSITIONS: Record<string, string> = {
  compacted: 'inserted',
  collected: 'compacted',
  atstation: 'collected',
  shredded: 'atstation',
}

// Stage → constructor number no Plutus
const STAGE_CONSTRUCTOR: Record<string, number> = {
  inserted: 0,
  compacted: 1,
  collected: 2,
  atstation: 3,
  shredded: 4,
}

// -------------------------------------------------------------------
// Helpers
// -------------------------------------------------------------------

async function cli(...args: string[]): Promise<string> {
  const { stdout } = await execFile('cardano-cli', args, {
    env: process.env,
    maxBuffer: 10 * 1024 * 1024,
  })
  return stdout.trim()
}

// cardano-cli 10.x retorna JSON em alguns comandos (ex: transaction txid → {"txhash":"abc..."})
function parseTxHash(raw: string): string {
  try {
    const parsed = JSON.parse(raw)
    if (parsed.txhash) return parsed.txhash
  } catch { /* not JSON, return as-is */ }
  return raw
}

function tmpFile(prefix: string): string {
  const rand = crypto.randomBytes(4).toString('hex')
  return path.join(os.tmpdir(), `greentoken-${prefix}-${rand}.json`)
}

async function readAddr(filePath: string): Promise<string> {
  return (await fs.readFile(filePath, 'utf8')).trim()
}

async function readPolicyId(): Promise<string> {
  return (await fs.readFile(paths.policyIdFile, 'utf8')).trim()
}

function tokenHex(): string {
  return Buffer.from('Greentoken').toString('hex')
}

// -------------------------------------------------------------------
// UTxO queries
// -------------------------------------------------------------------

interface UtxoValue {
  lovelace: number
  [policyId: string]: any
}

interface UtxoEntry {
  address: string
  value: UtxoValue
}

export async function queryUtxosJson(address: string): Promise<Record<string, UtxoEntry>> {
  const outFile = tmpFile('utxo')
  try {
    await cli('conway', 'query', 'utxo',
      '--address', address,
      '--testnet-magic', config.CARDANO_NODE_MAGIC,
      '--socket-path', config.CARDANO_NODE_SOCKET_PATH,
      '--out-file', outFile,
    )
    const data = await fs.readFile(outFile, 'utf8')
    return JSON.parse(data)
  } finally {
    await fs.unlink(outFile).catch(() => {})
  }
}

export async function isUtxoOnChain(txHash: string, txIndex: number): Promise<boolean> {
  const scriptAddr = await readAddr(paths.scriptAddr)
  const utxos = await queryUtxosJson(scriptAddr)
  return `${txHash}#${txIndex}` in utxos
}

export async function findOperatorUtxos(minLovelace: number): Promise<{ txIn: string; lovelace: number }[]> {
  const operatorAddr = await readAddr(paths.operatorAddr)
  const utxos = await queryUtxosJson(operatorAddr)

  const results: { txIn: string; lovelace: number }[] = []

  for (const [txIn, entry] of Object.entries(utxos)) {
    const keys = Object.keys(entry.value)
    const isAdaOnly = keys.length === 1 && keys[0] === 'lovelace'
    if (!isAdaOnly) continue

    const lovelace = entry.value.lovelace
    if (lovelace < minLovelace) continue
    results.push({ txIn, lovelace })
  }

  // Sort descending by lovelace so the largest is first
  results.sort((a, b) => b.lovelace - a.lovelace)
  return results
}

// -------------------------------------------------------------------
// Operator UTxO allocation pool
// -------------------------------------------------------------------
// Submissões concorrentes podem disputar o mesmo UTxO do operador (a query
// on-chain reflete um estado consistente, mas duas chamadas próximas vêem o
// mesmo set antes de qualquer tx ser submetida). Para evitar colisão:
//   - mutex serializa a SELEÇÃO (a submissão fica paralela)
//   - reserva em memória marca UTxOs já alocados por um TTL longo o suficiente
//     pra tx atingir o nó (após isso a query on-chain já não retorna o UTxO).

const RESERVATION_TTL_MS = 60_000
const reservedTxIns = new Set<string>()
let allocLock: Promise<void> = Promise.resolve()

// Aloca até `count` UTxOs do operador (pode retornar menos se houver escassez -
// callers em batch tratam best-effort; callers singulares devem checar o tamanho
// do array ou usar allocateOperatorUtxo).
export async function allocateOperatorUtxos(
  count: number,
  minLovelace: number,
): Promise<{ txIn: string; lovelace: number }[]> {
  let release!: () => void
  const next = new Promise<void>(resolve => { release = resolve })
  const prev = allocLock
  allocLock = next
  await prev

  try {
    const all = await findOperatorUtxos(minLovelace)
    const available = all.filter(u => !reservedTxIns.has(u.txIn))
    const allocated = available.slice(0, count)
    for (const u of allocated) {
      reservedTxIns.add(u.txIn)
      setTimeout(() => reservedTxIns.delete(u.txIn), RESERVATION_TTL_MS).unref()
    }
    return allocated
  } finally {
    release()
  }
}

async function allocateOperatorUtxo(minLovelace: number): Promise<{ txIn: string; lovelace: number }> {
  const [utxo] = await allocateOperatorUtxos(1, minLovelace)
  if (!utxo) {
    throw new Error(`Nenhum UTxO do operador disponível (>= ${minLovelace} lovelace)`)
  }
  return utxo
}

// -------------------------------------------------------------------
// Datum generation
// -------------------------------------------------------------------

function buildDatumJson(userPubkeyHash: string, bottleHex: string, stage: string): object {
  return {
    constructor: 0,
    fields: [
      { bytes: userPubkeyHash },
      { bytes: bottleHex },
      { constructor: STAGE_CONSTRUCTOR[stage], fields: [] },
    ],
  }
}

function buildRedeemerJson(targetStage: string): object {
  return {
    constructor: 0,
    fields: [
      { constructor: STAGE_CONSTRUCTOR[targetStage], fields: [] },
    ],
  }
}

export async function generateDatums(bottleId: string, userPubkeyHash: string): Promise<void> {
  const bottleHex = Buffer.from(bottleId).toString('hex')
  const dir = path.join(paths.datumDir, bottleId)
  await fs.mkdir(dir, { recursive: true })

  for (const stage of Object.keys(STAGE_CONSTRUCTOR)) {
    const datum = buildDatumJson(userPubkeyHash, bottleHex, stage)
    const filePath = path.join(dir, `datum-${bottleId}-${stage}.json`)
    await fs.writeFile(filePath, JSON.stringify(datum, null, 2))
  }
}

// -------------------------------------------------------------------
// Transaction: create bottle
// -------------------------------------------------------------------

export async function createBottle(params: {
  bottleId: string
  userAddr: string
  userPubkeyHash: string
}): Promise<string> {
  const { bottleId, userAddr, userPubkeyHash } = params

  // Gera datums para todos os estagios
  await generateDatums(bottleId, userPubkeyHash)

  const scriptAddr = await readAddr(paths.scriptAddr)
  const operatorAddr = await readAddr(paths.operatorAddr)
  const policyId = await readPolicyId()
  const tokHex = tokenHex()
  const datumFile = path.join(paths.datumDir, bottleId, `datum-${bottleId}-inserted.json`)
  const mintRedeemerFile = path.join(paths.redeemerDir, 'mint-inserted.json')

  const { txIn } = await allocateOperatorUtxo(5_000_000)

  await fs.mkdir(paths.txDir, { recursive: true })
  const bodyFile = path.join(paths.txDir, `mint-bottle-${bottleId}.body`)
  const signedFile = path.join(paths.txDir, `mint-bottle-${bottleId}.signed`)

  await cli('conway', 'transaction', 'build',
    '--testnet-magic', config.CARDANO_NODE_MAGIC,
    '--socket-path', config.CARDANO_NODE_SOCKET_PATH,
    '--tx-in', txIn,
    '--tx-in-collateral', txIn,
    '--tx-out', `${scriptAddr}+2000000`,
    '--tx-out-inline-datum-file', datumFile,
    '--tx-out', `${userAddr}+2000000 + 10 ${policyId}.${tokHex}`,
    '--change-address', operatorAddr,
    '--mint', `10 ${policyId}.${tokHex}`,
    '--mint-script-file', paths.policyPlutus,
    '--mint-redeemer-file', mintRedeemerFile,
    '--out-file', bodyFile,
  )

  await cli('conway', 'transaction', 'sign',
    '--tx-body-file', bodyFile,
    '--signing-key-file', paths.operatorSkey,
    '--testnet-magic', config.CARDANO_NODE_MAGIC,
    '--out-file', signedFile,
  )

  // Tx hash
  const txHashRaw = await cli('conway', 'transaction', 'txid',
    '--tx-file', signedFile,
  )
  const txHash = parseTxHash(txHashRaw)

  // Submit
  await cli('conway', 'transaction', 'submit',
    '--tx-file', signedFile,
    '--testnet-magic', config.CARDANO_NODE_MAGIC,
    '--socket-path', config.CARDANO_NODE_SOCKET_PATH,
  )

  return txHash
}

// -------------------------------------------------------------------
// Transaction: advance stage
// -------------------------------------------------------------------

export async function advanceStage(params: {
  bottleId: string
  targetStage: string
  userAddr: string
  utxoHash: string
  utxoIndex: number
  operatorTxIn?: string
}): Promise<string> {
  const { bottleId, targetStage, userAddr, utxoHash, utxoIndex } = params

  const sourceStage = TRANSITIONS[targetStage]
  if (!sourceStage) throw new Error(`Transicao invalida para estagio: ${targetStage}`)

  const scriptAddr = await readAddr(paths.scriptAddr)
  const operatorAddr = await readAddr(paths.operatorAddr)
  const policyId = await readPolicyId()
  const tokHex = tokenHex()
  const rewardAmount = REWARDS_BY_STAGE[targetStage]

  const datumOut = path.join(paths.datumDir, bottleId, `datum-${bottleId}-${targetStage}.json`)
  const redeemerFile = path.join(paths.redeemerDir, `redeemer-${sourceStage}-to-${targetStage}.json`)
  const mintRedeemerFile = path.join(paths.redeemerDir, `mint-${targetStage}.json`)

  await fs.access(datumOut)
  await fs.access(redeemerFile)
  await fs.access(mintRedeemerFile)

  const collateralTxIn = params.operatorTxIn ?? (await allocateOperatorUtxo(4_000_000)).txIn
  const bottleTxIn = `${utxoHash}#${utxoIndex}`

  await fs.mkdir(paths.txDir, { recursive: true })
  const bodyFile = path.join(paths.txDir, `tx-advance-${bottleId}-${targetStage}.body`)
  const signedFile = path.join(paths.txDir, `tx-advance-${bottleId}-${targetStage}.signed`)

  await cli('conway', 'transaction', 'build',
    '--testnet-magic', config.CARDANO_NODE_MAGIC,
    '--socket-path', config.CARDANO_NODE_SOCKET_PATH,
    '--tx-in', bottleTxIn,
    '--tx-in-script-file', paths.scriptFile,
    '--tx-in-inline-datum-present',
    '--tx-in-redeemer-file', redeemerFile,
    '--tx-in', collateralTxIn,
    '--tx-in-collateral', collateralTxIn,
    '--tx-out', `${scriptAddr}+2000000`,
    '--tx-out-inline-datum-file', datumOut,
    '--tx-out', `${userAddr}+2000000 + ${rewardAmount} ${policyId}.${tokHex}`,
    '--change-address', operatorAddr,
    '--mint', `${rewardAmount} ${policyId}.${tokHex}`,
    '--mint-script-file', paths.policyPlutus,
    '--mint-redeemer-file', mintRedeemerFile,
    '--out-file', bodyFile,
  )

  await cli('conway', 'transaction', 'sign',
    '--tx-body-file', bodyFile,
    '--signing-key-file', paths.operatorSkey,
    '--testnet-magic', config.CARDANO_NODE_MAGIC,
    '--out-file', signedFile,
  )

  // Tx hash
  const txHashRaw = await cli('conway', 'transaction', 'txid',
    '--tx-file', signedFile,
  )
  const txHash = parseTxHash(txHashRaw)

  // Submit
  await cli('conway', 'transaction', 'submit',
    '--tx-file', signedFile,
    '--testnet-magic', config.CARDANO_NODE_MAGIC,
    '--socket-path', config.CARDANO_NODE_SOCKET_PATH,
  )

  return txHash
}
