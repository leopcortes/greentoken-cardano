# Backend - Greentoken Cardano

Serviço off-chain responsável por gerenciar o banco de dados, a comunicação com a blockchain Cardano e a lógica de recompensas em Greentoken.

## Estrutura

```
backend/
├── db/
│   ├── schema.sql        # Script de criação das tabelas
│   └── migrations/       # Futuras alterações no schema
├── src/
│   ├── routes/           # Endpoints da API
│   ├── services/         # Lógica de negócio (blockchain, rewards...)
│   └── index.ts          # Entry point
├── package.json
└── tsconfig.json
```

## Banco de Dados

### Visão Geral

O schema foi projetado para espelhar o ciclo de vida completo de uma garrafa no sistema - desde o depósito no container até a trituragem final - integrando os dados off-chain com o estado on-chain do contrato Plutus.

---

### Tabela `users`

Armazena os dois tipos de usuário do sistema. O campo `role` diferencia:

- `recycler` - usuário que deposita garrafas e recebe Greentoken como recompensa
- `owner` - dono dos pontos de reciclagem, com acesso aos containers, caminhões e rotas

A estrutura suporta múltiplos owners no futuro, embora o sistema atual opere com um único owner.

```sql
CREATE TABLE users (
  id             UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  role           VARCHAR(10) NOT NULL CHECK (role IN ('recycler', 'owner')),
  name           VARCHAR(255) NOT NULL,
  email          VARCHAR(255) UNIQUE NOT NULL,
  wallet_address VARCHAR(255) NOT NULL,
  pubkey_hash    VARCHAR(255) NOT NULL,
  created_at     TIMESTAMP DEFAULT NOW()
);
```

---

### Tabela `containers`

Representa os pontos físicos de coleta de garrafas. Cada container pertence a um owner e possui controle de volume para determinar quando está cheio.

Os campos `current_volume_liters` e `capacity_liters` permitem calcular a porcentagem de preenchimento. O campo `status` controla o ciclo de vida do container:

- `active` - em operação normal, recebendo garrafas
- `full` - capacidade atingida, aciona a lógica de rota do caminhão
- `in_route` - já adicionado à rota de coleta de um caminhão
- `maintenance` - fora de operação temporariamente

```sql
CREATE TABLE containers (
  id                    UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  owner_id              UUID NOT NULL REFERENCES users(id),
  name                  VARCHAR(255) NOT NULL,
  location_name         VARCHAR(255),
  latitude              FLOAT,
  longitude             FLOAT,
  capacity_liters       INT NOT NULL,
  current_volume_liters INT NOT NULL DEFAULT 0,
  status                VARCHAR(20) NOT NULL DEFAULT 'active'
                          CHECK (status IN ('active', 'full', 'in_route', 'maintenance')),
  last_updated          TIMESTAMP DEFAULT NOW()
);
```

---

### Tabelas `trucks`, `routes` e `route_stops`

Implementam a lógica de **rota inteligente** da frota de caminhões. Um caminhão só é enviado a um container quando ele atinge o status `full`, evitando deslocamentos desnecessários.

O fluxo funciona assim: quando um container fica `full`, um novo `route_stop` é criado e vinculado à rota ativa do caminhão. O campo `stop_order` define a sequência otimizada de coleta. Ao ser coletado, o container volta para o status `active`.

```sql
CREATE TABLE trucks (
  id            UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  license_plate VARCHAR(20) UNIQUE NOT NULL,
  status        VARCHAR(20) NOT NULL DEFAULT 'available'
                  CHECK (status IN ('available', 'on_route', 'maintenance')),
  last_updated  TIMESTAMP DEFAULT NOW()
);

CREATE TABLE routes (
  id           UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  truck_id     UUID NOT NULL REFERENCES trucks(id),
  status       VARCHAR(20) NOT NULL DEFAULT 'planned'
                 CHECK (status IN ('planned', 'in_progress', 'completed')),
  created_at   TIMESTAMP DEFAULT NOW(),
  completed_at TIMESTAMP
);

CREATE TABLE route_stops (
  id           UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  route_id     UUID NOT NULL REFERENCES routes(id),
  container_id UUID NOT NULL REFERENCES containers(id),
  stop_order   INT NOT NULL,
  status       VARCHAR(20) NOT NULL DEFAULT 'pending'
                 CHECK (status IN ('pending', 'collected')),
  collected_at TIMESTAMP
);
```

---

### Tabela `bottles`

Espelha os estágios do contrato Plutus on-chain. Cada linha representa uma garrafa e seu progresso na cadeia de reciclagem:

```
inserted → compacted → collected → atstation → shredded
```

Os campos `utxo_hash` e `utxo_index` são essenciais para o serviço off-chain localizar o UTxO correto na blockchain ao submeter uma nova transação.

```sql
CREATE TABLE bottles (
  id             UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id        UUID NOT NULL REFERENCES users(id),
  container_id   UUID REFERENCES containers(id),
  bottle_id_text VARCHAR(255) NOT NULL,
  bottle_id_hex  VARCHAR(255) NOT NULL,
  current_stage  VARCHAR(20) NOT NULL DEFAULT 'inserted'
                   CHECK (current_stage IN ('inserted','compacted','collected','atstation','shredded')),
  utxo_hash      VARCHAR(255),
  utxo_index     INT,
  inserted_at    TIMESTAMP DEFAULT NOW(),
  compacted_at   TIMESTAMP,
  collected_at   TIMESTAMP,
  atstation_at   TIMESTAMP,
  shredded_at    TIMESTAMP
);
```

---

### Tabela `blockchain_txs`

Log de auditoria de todas as transações submetidas à blockchain. Os campos `datum_json` e `redeemer_json` preservam exatamente o que foi enviado ao contrato Plutus, permitindo rastreabilidade completa - fundamental para a análise na tese.

```sql
CREATE TABLE blockchain_txs (
  id            UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  bottle_id     UUID NOT NULL REFERENCES bottles(id),
  stage         VARCHAR(20) NOT NULL,
  tx_hash       VARCHAR(255),
  status        VARCHAR(20) NOT NULL DEFAULT 'pending'
                  CHECK (status IN ('pending', 'confirmed', 'failed')),
  datum_json    TEXT,
  redeemer_json TEXT,
  submitted_at  TIMESTAMP DEFAULT NOW(),
  confirmed_at  TIMESTAMP
);
```

---

### Tabela `rewards`

Registra cada envio de Greentoken para um reciclador. A quantidade de tokens enviada por estágio segue a tabela de recompensas definida na política do token:

| Estágio     | Greentoken |
|-------------|-----------|
| `inserted`  | 10        |
| `compacted` | 5         |
| `collected` | 5         |
| `atstation` | 10        |
| `shredded`  | 10        |

```sql
CREATE TABLE rewards (
  id                UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id           UUID NOT NULL REFERENCES users(id),
  bottle_id         UUID NOT NULL REFERENCES bottles(id),
  tx_id             UUID REFERENCES blockchain_txs(id),
  stage             VARCHAR(20) NOT NULL,
  greentoken_amount INT NOT NULL,
  tx_hash           VARCHAR(255),
  sent_at           TIMESTAMP DEFAULT NOW()
);
```

---

## Fluxo Completo

```
Usuário deposita garrafa
        ↓
Container registra volume
        ↓
Container fica full → adicionado à rota do caminhão
        ↓
Backend monta datum + redeemer → submete tx na Cardano
        ↓
blockchain_txs registra a tx → aguarda confirmação
        ↓
Confirmação recebida → rewards envia Greentoken ao usuário
```

## Tecnologias

- **Banco de dados**: PostgreSQL
- **Backend**: Node.js + TypeScript
- **Blockchain**: Cardano Preprod (testnet) via `cardano-cli`
- **Token**: Greentoken (política de assinatura única, sem time-lock)