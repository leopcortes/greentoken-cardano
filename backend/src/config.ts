import * as path from 'path'
import 'dotenv/config'

function requireEnv(name: string): string {
  const val = process.env[name]
  if (!val) throw new Error(`Variavel de ambiente obrigatoria nao definida: ${name}`)
  return val
}

export const config = {
  PORT: parseInt(process.env.PORT || '3000', 10),
  DATABASE_URL: requireEnv('DATABASE_URL'),
  CARDANO_NODE_SOCKET_PATH: requireEnv('CARDANO_NODE_SOCKET_PATH'),
  CARDANO_NODE_MAGIC: requireEnv('CARDANO_NODE_MAGIC'),
  PROJECT_ROOT: process.env.PROJECT_ROOT || path.resolve(__dirname, '../..'),
  CONFIRMATION_POLL_MS: parseInt(process.env.CONFIRMATION_POLL_MS || '5000', 10),
  BLOCKFROST_API_KEY: requireEnv('BLOCKFROST_API_KEY'),
  WALLET_ENCRYPTION_KEY: requireEnv('WALLET_ENCRYPTION_KEY'),
}

// Caminhos derivados (relativos ao PROJECT_ROOT)
export const paths = {
  scriptFile: path.join(config.PROJECT_ROOT, 'assets/bottle-validator.plutus'),
  scriptAddr: path.join(config.PROJECT_ROOT, 'assets/wallet/bottle.addr'),
  operatorAddr: path.join(config.PROJECT_ROOT, 'assets/wallet/payment.addr'),
  operatorSkey: path.join(config.PROJECT_ROOT, 'assets/wallet/payment.skey'),
  policyPlutus: path.join(config.PROJECT_ROOT, 'assets/greentoken-policy.plutus'),
  policyIdFile: path.join(config.PROJECT_ROOT, 'assets/policy/policyID'),
  datumDir: path.join(config.PROJECT_ROOT, 'assets/datums'),
  redeemerDir: path.join(config.PROJECT_ROOT, 'assets/redeemers'),
  txDir: path.join(config.PROJECT_ROOT, 'assets/txs'),
}
