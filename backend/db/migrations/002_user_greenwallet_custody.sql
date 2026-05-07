-- Greenwallet: custodia de mnemonicas BIP-39 criptografadas (AES-256-GCM)
-- A chave-mestre vive em WALLET_ENCRYPTION_KEY (env). Cada linha guarda
-- ciphertext + iv (12B) + auth_tag (16B) + versao para permitir rotacao.
-- wallet_address e pubkey_hash deixam de ser NOT NULL para permitir users
-- legados criados sem greenwallet (fluxo antigo de address manual).

ALTER TABLE users
  ADD COLUMN IF NOT EXISTS mnemonic_ciphertext     BYTEA,
  ADD COLUMN IF NOT EXISTS mnemonic_iv             BYTEA,
  ADD COLUMN IF NOT EXISTS mnemonic_auth_tag       BYTEA,
  ADD COLUMN IF NOT EXISTS encryption_key_version  SMALLINT;

ALTER TABLE users
  ALTER COLUMN wallet_address DROP NOT NULL,
  ALTER COLUMN pubkey_hash    DROP NOT NULL;
