import express from 'express'
import { config } from './config'
import { pool } from './db/pool'
import { startConfirmationWorker } from './workers/confirmation.worker'
import { router as authRouter } from './routes/auth.routes'
import { router as usersRouter } from './routes/users.routes'
import { router as bottlesRouter } from './routes/bottles.routes'
import { router as containersRouter } from './routes/containers.routes'
import { router as trucksRouter } from './routes/trucks.routes'
import { router as routesRouter } from './routes/routes.routes'
import { router as stationsRouter } from './routes/stations.routes'
import { router as operatorRouter } from './routes/operator.routes'
import * as usersDb from './db/queries/users'
import { hashPassword } from './auth/passwords'

const app = express()

app.use(express.json())

// Rotas
app.use('/auth', authRouter)
app.use('/users', usersRouter)
app.use('/bottles', bottlesRouter)
app.use('/containers', containersRouter)
app.use('/trucks', trucksRouter)
app.use('/routes', routesRouter)
app.use('/stations', stationsRouter)
app.use('/operator', operatorRouter)

// Health check
app.get('/health', async (_req, res) => {
  try {
    await pool.query('SELECT 1')
    res.json({ status: 'ok', db: 'connected' })
  } catch {
    res.status(503).json({ status: 'error', db: 'disconnected' })
  }
})

// Garante que existe um owner com password_hash. Cobre 3 cenarios:
//   1) Não existe owner com OWNER_EMAIL -> cria.
//   2) Existe mas password_hash IS NULL (user legado) -> define a senha.
//   3) Ja tem hash -> no-op.
async function bootstrapOwner() {
  const email = config.OWNER_EMAIL
  const existing = await usersDb.getAuthRecord(email)
  if (!existing) {
    const hash = await hashPassword(config.OWNER_PASSWORD)
    await usersDb.create({
      role: 'owner',
      name: 'Admin Greentoken',
      email,
      password_hash: hash,
    })
    console.log(`[bootstrap] Owner criado: ${email}`)
    return
  }
  if (existing.role !== 'owner') {
    console.warn(`[bootstrap] Usuário com email ${email} existe mas não e owner - ignorando`)
    return
  }
  if (!existing.password_hash) {
    const hash = await hashPassword(config.OWNER_PASSWORD)
    await usersDb.setPasswordHash(existing.id, hash)
    console.log(`[bootstrap] Senha do owner ${email} definida via OWNER_PASSWORD`)
    return
  }
  console.log(`[bootstrap] Owner ${email} ja configurado`)
}

// Startup
async function main() {
  // Testa conexao com o banco
  try {
    await pool.query('SELECT 1')
    console.log('[db] Conectado ao PostgreSQL')
  } catch (err) {
    console.error('[db] Falha ao conectar:', err)
    process.exit(1)
  }

  try {
    await bootstrapOwner()
  } catch (err) {
    console.error('[bootstrap] Falha ao configurar owner:', err)
    process.exit(1)
  }

  // Inicia o worker de confirmacao
  startConfirmationWorker()

  app.listen(config.PORT, () => {
    console.log(`[server] Rodando em http://localhost:${config.PORT}`)
  })
}

main()
