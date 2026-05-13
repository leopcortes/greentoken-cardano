-- Extensão para geração de UUIDs
CREATE EXTENSION IF NOT EXISTS "pgcrypto";

-- recyclers e owner
-- A greenwallet (mnemonica + endereço) e gerada no backend ao criar
-- usuarios novos. wallet_address/pubkey_hash sao NULL apenas para users
-- legados criados antes da migracao 002 (sem greenwallet).
CREATE TABLE users (
  id                     UUID        PRIMARY KEY DEFAULT gen_random_uuid(),
  role                   VARCHAR(10) NOT NULL CHECK (role IN ('recycler', 'owner')),
  name                   VARCHAR(255) NOT NULL,
  email                  VARCHAR(255) UNIQUE NOT NULL,
  wallet_address         VARCHAR(255),
  pubkey_hash            VARCHAR(255),
  mnemonic_ciphertext    BYTEA,
  mnemonic_iv            BYTEA,
  mnemonic_auth_tag      BYTEA,
  encryption_key_version SMALLINT,
  created_at             TIMESTAMP   NOT NULL DEFAULT NOW()
);

-- Cada container pertence a um owner
CREATE TABLE containers (
  id                    UUID         PRIMARY KEY DEFAULT gen_random_uuid(),
  owner_id              UUID         NOT NULL REFERENCES users(id) ON DELETE RESTRICT,
  name                  VARCHAR(255) NOT NULL,
  location_name         VARCHAR(255),
  latitude              FLOAT,
  longitude             FLOAT,
  capacity_liters       NUMERIC(10,2) NOT NULL,
  current_volume_liters NUMERIC(12,5) NOT NULL DEFAULT 0,
  status                VARCHAR(30)  NOT NULL DEFAULT 'active'
                          CHECK (status IN ('active', 'ready_for_collection', 'in_route', 'maintenance')),
  last_updated          TIMESTAMP    NOT NULL DEFAULT NOW(),
  UNIQUE (owner_id, name)
);

-- Caminhoes
CREATE TABLE trucks (
  id            UUID        PRIMARY KEY DEFAULT gen_random_uuid(),
  license_plate VARCHAR(20) UNIQUE NOT NULL,
  status        VARCHAR(20) NOT NULL DEFAULT 'available'
                  CHECK (status IN ('available', 'on_route', 'maintenance')),
  last_updated  TIMESTAMP   NOT NULL DEFAULT NOW()
);

-- Estacoes de tratamento (destino final das rotas)
CREATE TABLE stations (
  id            UUID         PRIMARY KEY DEFAULT gen_random_uuid(),
  name          VARCHAR(255) NOT NULL,
  location_name VARCHAR(255),
  latitude      FLOAT,
  longitude     FLOAT,
  created_at    TIMESTAMP    NOT NULL DEFAULT NOW(),
  UNIQUE (name, location_name)
);

-- Cada rota representa uma saida de um caminhao
-- station_id = estacao de destino onde as garrafas serao entregues
CREATE TABLE routes (
  id           UUID        PRIMARY KEY DEFAULT gen_random_uuid(),
  truck_id     UUID        NOT NULL REFERENCES trucks(id) ON DELETE RESTRICT,
  station_id   UUID        REFERENCES stations(id) ON DELETE RESTRICT,
  status       VARCHAR(20) NOT NULL DEFAULT 'planned'
                 CHECK (status IN ('planned', 'in_progress', 'awaiting_delivery', 'completed')),
  created_at   TIMESTAMP   NOT NULL DEFAULT NOW(),
  completed_at TIMESTAMP
);

-- um container por parada
-- Container só entra na rota quando status = 'ready_for_collection'
CREATE TABLE route_stops (
  id           UUID        PRIMARY KEY DEFAULT gen_random_uuid(),
  route_id     UUID        NOT NULL REFERENCES routes(id) ON DELETE CASCADE,
  container_id UUID        NOT NULL REFERENCES containers(id) ON DELETE RESTRICT,
  stop_order   INT         NOT NULL,
  status       VARCHAR(20) NOT NULL DEFAULT 'pending'
                 CHECK (status IN ('pending', 'collected')),
  collected_at TIMESTAMP
);

-- bottles
-- espelha os estagios do contrato plutus on-chain.
-- utxo_hash + utxo_index identificam o UTxO na blockchain.
-- container_id: container onde a garrafa esta (inserted/compacted)
-- route_id: rota/caminhao transportando a garrafa (collected)
-- station_id: estacao de tratamento onde a garrafa esta (atstation/shredded)
CREATE TABLE bottles (
  id             UUID         PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id        UUID         NOT NULL REFERENCES users(id) ON DELETE RESTRICT,
  container_id   UUID         REFERENCES containers(id) ON DELETE SET NULL,
  route_id       UUID         REFERENCES routes(id) ON DELETE SET NULL,
  station_id     UUID         REFERENCES stations(id) ON DELETE SET NULL,
  bottle_id_text VARCHAR(255) NOT NULL,
  bottle_id_hex  VARCHAR(255) NOT NULL,
  volume_ml      NUMERIC(10,1) NOT NULL DEFAULT 500,
  current_stage  VARCHAR(20)  NOT NULL DEFAULT 'inserted'
                   CHECK (current_stage IN ('inserted','compacted','collected','atstation','shredded')),
  utxo_hash      VARCHAR(255),
  utxo_index     INT,
  inserted_at    TIMESTAMP    NOT NULL DEFAULT NOW(),
  compacted_at   TIMESTAMP,
  collected_at   TIMESTAMP,
  atstation_at   TIMESTAMP,
  shredded_at    TIMESTAMP
);

-- blockchain_txs
-- log de auditoria de todas as transações submetidas
-- datum_json e redeemer_json preservam o que foi enviado ao contrato.
CREATE TABLE blockchain_txs (
  id            UUID        PRIMARY KEY DEFAULT gen_random_uuid(),
  bottle_id     UUID        NOT NULL REFERENCES bottles(id) ON DELETE RESTRICT,
  stage         VARCHAR(20) NOT NULL
                  CHECK (stage IN ('inserted','compacted','collected','atstation','shredded')),
  tx_hash       VARCHAR(255),
  status        VARCHAR(20) NOT NULL DEFAULT 'pending'
                  CHECK (status IN ('pending', 'confirmed', 'failed')),
  datum_json    TEXT,
  redeemer_json TEXT,
  submitted_at  TIMESTAMP   NOT NULL DEFAULT NOW(),
  confirmed_at  TIMESTAMP
);

-- Greentokens enviados ao reciclador por estagio concluido.
-- inserted=10, compacted=3, collected=7, atstation=10, shredded=20 (total=50)
CREATE TABLE rewards (
  id                UUID        PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id           UUID        NOT NULL REFERENCES users(id) ON DELETE RESTRICT,
  bottle_id         UUID        NOT NULL REFERENCES bottles(id) ON DELETE RESTRICT,
  tx_id             UUID        REFERENCES blockchain_txs(id) ON DELETE SET NULL,
  stage             VARCHAR(20) NOT NULL
                      CHECK (stage IN ('inserted','compacted','collected','atstation','shredded')),
  greentoken_amount INT         NOT NULL,
  tx_hash           VARCHAR(255),
  sent_at           TIMESTAMP   NOT NULL DEFAULT NOW()
);

-- indices

-- Buscar garrafas por usuario
CREATE INDEX idx_bottles_user_id       ON bottles(user_id);

-- Buscar garrafas por container
CREATE INDEX idx_bottles_container_id  ON bottles(container_id);

-- Buscar garrafas por estagio atual
CREATE INDEX idx_bottles_stage         ON bottles(current_stage);

-- Buscar UTxO específico usado pelo servico off-chain
CREATE INDEX idx_bottles_utxo          ON bottles(utxo_hash, utxo_index);

-- Buscar txs por status pending to polling de confirmacao
CREATE INDEX idx_blockchain_txs_status ON blockchain_txs(status);

-- Buscar txs por garrafa
CREATE INDEX idx_blockchain_txs_bottle ON blockchain_txs(bottle_id);

-- Buscar recompensas por usuario
CREATE INDEX idx_rewards_user_id       ON rewards(user_id);

-- Buscar garrafas por rota (coleta em andamento)
CREATE INDEX idx_bottles_route_id      ON bottles(route_id);

-- Buscar garrafas por estacao de tratamento
CREATE INDEX idx_bottles_station_id    ON bottles(station_id);

-- Buscar containers por status (detectar containers 'ready_for_collection')
CREATE INDEX idx_containers_status     ON containers(status);

-- Buscar paradas pendentes de uma rota
CREATE INDEX idx_route_stops_route     ON route_stops(route_id, status);

-- seed
-- Apenas o owner do sistema e os recursos físicos (containers, estações, caminhões).
-- Recicladores são criados pelo owner via dashboard após o setup.
-- A greenwallet do owner é gerada via dashboard após o primeiro login
-- (POST /users/:id/greenwallet/migrate ou criação via painel de greenwallets).

INSERT INTO users (role, name, email)
VALUES (
  'owner',
  'Admin Greentoken',
  'owner@greentoken.io'
)
ON CONFLICT (email) DO NOTHING;

INSERT INTO containers (owner_id, name, location_name, latitude, longitude, capacity_liters, status)
VALUES (
  (SELECT id FROM users WHERE email = 'owner@greentoken.io'),
  'Ponto de Coleta FT UnB',
  'Asa Norte, Brasília',
  -15.762412,
  -47.872549,
  10.00,
  'active'
)
ON CONFLICT (owner_id, name) DO NOTHING;

INSERT INTO containers (owner_id, name, location_name, latitude, longitude, capacity_liters, status)
VALUES (
  (SELECT id FROM users WHERE email = 'owner@greentoken.io'),
  'Ponto de Coleta BCE UnB',
  'Asa Norte, Brasília',
  -15.761322,
  -47.867377,
  5.00,
  'active'
)
ON CONFLICT (owner_id, name) DO NOTHING;

INSERT INTO containers (owner_id, name, location_name, latitude, longitude, capacity_liters, status)
VALUES (
  (SELECT id FROM users WHERE email = 'owner@greentoken.io'),
  'Ponto de Coleta Pão de Açucar',
  'Lago Norte, Brasília',
  -15.72296,
  -47.88760,
  100.00,
  'active'
)
ON CONFLICT (owner_id, name) DO NOTHING;

INSERT INTO containers (owner_id, name, location_name, latitude, longitude, capacity_liters, status)
VALUES (
  (SELECT id FROM users WHERE email = 'owner@greentoken.io'),
  'Ponto de Coleta BigBox',
  'Lago Norte, Brasília',
  -15.72074,
  -47.88310,
  50.00,
  'active'
)
ON CONFLICT (owner_id, name) DO NOTHING;

INSERT INTO stations (name, location_name, latitude, longitude)
VALUES (
  'Central de Reciclagem',
  'Lago Norte, Brasília',
  -15.714127,
  -47.874617
)
ON CONFLICT (name, location_name) DO NOTHING;

INSERT INTO stations (name, location_name, latitude, longitude)
VALUES (
  'Complexo Integrado de Reciclagem',
  'Zona Industrial, Brasília',
  -15.793054,
  -47.967047
)
ON CONFLICT (name, location_name) DO NOTHING;

INSERT INTO trucks (license_plate, status)
VALUES (
  'PBX-6480',
  'available'
)
ON CONFLICT (license_plate) DO NOTHING;

INSERT INTO trucks (license_plate, status)
VALUES (
  'XYZ-9876',
  'available'
)
ON CONFLICT (license_plate) DO NOTHING;

INSERT INTO trucks (license_plate, status)
VALUES (
  'ABC-1234',
  'available'
)
ON CONFLICT (license_plate) DO NOTHING;
