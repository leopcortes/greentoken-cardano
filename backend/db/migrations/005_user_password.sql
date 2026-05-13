-- Migration 005: senha por usuário para suportar login email+senha.
--
-- password_hash NULLABLE: usuarios pre-existentes (criados antes desta
-- migration) ficam sem senha ate que:
--   - owner: o bootstrap (backend/src/index.ts) preencha via env OWNER_PASSWORD
--   - recycler: o usuário faça /auth/signup com mode='restore_greenwallet'
--     usando as 24 palavras da greenwallet (sem essa rota, conta fica
--     inacessivel por email/senha).
--
-- Indice em LOWER(email) para case-insensitive lookups no login.
ALTER TABLE users
  ADD COLUMN IF NOT EXISTS password_hash VARCHAR(255);

CREATE INDEX IF NOT EXISTS idx_users_email_lower ON users (LOWER(email));
