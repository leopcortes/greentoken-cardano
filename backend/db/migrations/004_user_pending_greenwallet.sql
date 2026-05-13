-- Migration 004: greenwallet pendente para usuarios legados
--
-- Adiciona campos pending_* em users para permitir gerar uma greenwallet
-- nova antes de swap-ar para os campos definitivos. Necessario para migrar
-- Owner, Leo 1 e Leo 2 (criados antes da migration 002 / sem
-- mnemonic_ciphertext) sem perder os FKs em bottles, rewards,
-- blockchain_txs (que referenciam users.id, nao wallet_address).
--
-- Fluxo:
--   1. POST /users/:id/greenwallet/migrate   -> preenche pending_*
--   2. Usuario transfere GTs off-app (Lace -> novo endereco)
--   3. POST /users/:id/greenwallet/migrate/confirm -> swap pending_* -> definitivos
--
-- Cancelar = setar todos os pending_* para NULL.

ALTER TABLE users
  ADD COLUMN IF NOT EXISTS pending_wallet_address         VARCHAR(255),
  ADD COLUMN IF NOT EXISTS pending_pubkey_hash            VARCHAR(255),
  ADD COLUMN IF NOT EXISTS pending_mnemonic_ciphertext    BYTEA,
  ADD COLUMN IF NOT EXISTS pending_mnemonic_iv            BYTEA,
  ADD COLUMN IF NOT EXISTS pending_mnemonic_auth_tag      BYTEA,
  ADD COLUMN IF NOT EXISTS pending_encryption_key_version SMALLINT,
  ADD COLUMN IF NOT EXISTS migration_initiated_at         TIMESTAMP;
