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

// Health check
app.get('/health', async (_req, res) => {
  try {
    await pool.query('SELECT 1')
    res.json({ status: 'ok', db: 'connected' })
  } catch {
    res.status(503).json({ status: 'error', db: 'disconnected' })
  }
})

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

  // Inicia o worker de confirmacao
  startConfirmationWorker()

  app.listen(config.PORT, () => {
    console.log(`[server] Rodando em http://localhost:${config.PORT}`)
  })
}

main()
