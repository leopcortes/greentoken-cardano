# Greentoken Cardano

Sistema de **rastreamento de reciclagem de garrafas na blockchain Cardano** usando smart contracts Plutus V2. Cada garrafa passa por 5 estágios (`inserted → compacted → collected → atstation → shredded`), e o reciclador recebe **tokens Greentoken** como recompensa a cada transição.

---

## Arquitetura

| Camada | Tecnologia | Descrição |
|--------|-----------|-----------|
| **On-chain** | Haskell / Plutus V2 | Smart contract que valida transições de estágio |
| **Off-chain** | Bash + cardano-cli | Scripts para operar diretamente na blockchain |
| **Backend** | Node.js + TypeScript + PostgreSQL | API REST que integra blockchain e banco de dados |

### Fluxo de integração

```
  Script bash OU API REST
         │
         ├─ Submete transação na blockchain Cardano (cardano-cli)
         ├─ Registra garrafa + tx pendente no PostgreSQL
         │
         │   ~~~ confirmation worker (polling a cada 15s) ~~~
         │
         ├─ Detecta UTxO confirmado on-chain
         │   ├─ Atualiza tx → confirmed
         │   ├─ Atualiza garrafa → utxo_hash
         │   └─ Registra recompensa Greentoken
         │
         └─ Dados disponíveis via API e banco de dados
```

Tanto os **scripts bash** quanto a **API REST** gravam no banco de dados e submetem transações na blockchain. O **confirmation worker** do backend confirma as transações e credita as recompensas automaticamente.

---

## Recompensas por estágio

| Estágio | Greentoken |
|---------|-----------|
| inserted | 10 |
| compacted | 10 |
| collected | 5 |
| atstation | 10 |
| shredded | 20 |
| **Total por garrafa** | **55** |

---

## Estrutura do Projeto

```
greentoken-cardano/
├── onchain/src/Greentoken/
│   └── BottleValidator.hs            # Smart contract Plutus V2
├── offchain/test/Greentoken/
│   └── BottleValidatorSpec.hs        # Testes do validador
├── app/
│   ├── Main.hs                       # CLI para exportar o contrato
│   └── WriteBottleValidator.hs       # Serialização do script
├── assets/
│   ├── bottle-validator.plutus       # Contrato compilado
│   ├── policy/                       # Minting policy (policyID, script, chaves)
│   ├── redeemers/                    # Redeemers para cada transição de estágio
│   ├── wallet/                       # Endereço do operador e do script Plutus
│   └── users/                        # Endereços e chaves dos usuários
├── backend/
│   ├── db/schema.sql                 # Schema PostgreSQL (DDL + seed)
│   ├── src/                          # Backend Node.js/TypeScript
│   ├── .env.example                  # Template de variáveis de ambiente
│   ├── package.json
│   └── tsconfig.json
├── scripts/
│   ├── _db-helper.sh                 # Helper: integração scripts ↔ PostgreSQL
│   ├── setup-wallet.sh               # Setup: chaves do operador + endereço do script
│   ├── setup-policy.sh               # Setup: minting policy + policyID
│   ├── import-user.sh                # Importa carteira externa (ex: Lace)
│   ├── create-user.sh                # Gera novo usuário via cardano-cli
│   ├── create-bottle.sh              # Cria garrafa (mint + contrato + banco)
│   ├── advance-stage.sh              # Avança estágio (transição + banco)
│   ├── query-bottle.sh               # Consulta UTxOs no script Plutus
│   └── query-balance.sh              # Consulta saldo de qualquer endereço
├── SETUP-LOCAL.md                    # Guia completo de configuração local
├── plutus-greentoken.cabal           # Configuração Haskell
└── cabal.project                     # Dependências Haskell
```

---

## Scripts

### Configuração inicial (executar uma vez)

| Script | Função | Uso |
|--------|--------|-----|
| `setup-wallet.sh` | Gera chaves do operador e endereço do script | `scripts/setup-wallet.sh` |
| `setup-policy.sh` | Gera minting policy e policyID | `scripts/setup-policy.sh` |

### Operação

| Script | Função | Uso |
|--------|--------|-----|
| `import-user.sh` | Importa carteira externa (Lace, etc.) | `scripts/import-user.sh <ID> <ADDR> [NOME] [EMAIL]` |
| `create-user.sh` | Cria usuário com chaves cardano-cli | `scripts/create-user.sh <ID> [NOME] [EMAIL]` |
| `create-bottle.sh` | Cria garrafa no contrato + banco | `scripts/create-bottle.sh <BOTTLE_ID> <USER_ID>` |
| `advance-stage.sh` | Avança estágio de uma garrafa | `scripts/advance-stage.sh <STAGE> <BOTTLE_ID> <USER_ADDR> <TX_IN>` |

### Consulta

| Script | Função | Uso |
|--------|--------|-----|
| `query-bottle.sh` | UTxOs no endereço do script | `scripts/query-bottle.sh [TX_HASH]` |
| `query-balance.sh` | Saldo de qualquer endereço | `scripts/query-balance.sh [ADDR\|USER_ID]` |

Todos os scripts de operação (`create-user`, `import-user`, `create-bottle`, `advance-stage`) gravam automaticamente no PostgreSQL quando o backend está configurado (leem `DATABASE_URL` de `backend/.env`).

---

## API REST

O backend roda na porta 3000 e expõe os seguintes endpoints:

| Método | Rota | Descrição |
|--------|------|-----------|
| `GET` | `/health` | Health check |
| `GET` | `/users` | Lista usuários (`?role=recycler\|owner`) |
| `GET` | `/users/:id` | Detalhe de um usuário |
| `POST` | `/users` | Cria usuário |
| `GET` | `/users/:id/rewards` | Recompensas + total Greentoken |
| `GET` | `/bottles` | Lista garrafas (`?user_id=`, `?stage=`, `?container_id=`) |
| `GET` | `/bottles/:id` | Detalhe + histórico (txs + rewards) |
| `POST` | `/bottles` | Cria garrafa (blockchain + banco) |
| `POST` | `/bottles/:id/advance` | Avança estágio |
| `GET` | `/containers` | Lista containers (`?status=`, `?owner_id=`) |
| `POST` | `/containers` | Cria container |
| `POST` | `/containers/:id/deposit` | Registra volume depositado |
| `POST` | `/containers/:id/collected` | Marca como coletado |
| `GET` | `/containers/status/full` | Containers cheios (prontos para rota) |

---

## Banco de Dados

Schema com 8 tabelas (`backend/db/schema.sql`):

- **`users`** — recicladores e donos de pontos de coleta
- **`containers`** — pontos físicos de coleta (volume, status)
- **`trucks`** — frota de caminhões
- **`routes`** / **`route_stops`** — rotas de coleta
- **`bottles`** — espelha os estágios do contrato Plutus (`utxo_hash` + `utxo_index`)
- **`blockchain_txs`** — log de auditoria de todas as transações
- **`rewards`** — registro de Greentoken enviados por estágio

---

## Quick Start

Consulte o [SETUP-LOCAL.md](SETUP-LOCAL.md) para o guia completo. Resumo rápido:

```bash
# 1. Configurar PostgreSQL
sudo -u postgres psql -c "CREATE DATABASE greentoken_db;"
psql -U postgres -d greentoken_db -f backend/db/schema.sql

# 2. Gerar chaves
export CARDANO_NODE_SOCKET_PATH=~/cardano/preprod/node.socket
export CARDANO_NODE_MAGIC=1
scripts/setup-wallet.sh
scripts/setup-policy.sh

# 3. Enviar tADA ao operador (via faucet ou carteira Lace)
cat assets/wallet/payment.addr

# 4. Iniciar nó e backend
cardano-start
cd backend && cp .env.example .env && npm install && npm run dev

# 5. Importar carteira e criar garrafa
scripts/import-user.sh user1 addr_test1q... "Nome" "email@test.com"
scripts/create-bottle.sh garrafa-001 user1
```

---

## Próximos Passos

- [ ] Implementar lógica de rotas de caminhão (container `full` → `route_stop` automático)
- [ ] Adicionar autenticação na API (JWT ou API keys)
- [ ] Testes automatizados para o backend
- [ ] Frontend web para recicladores e owners
- [ ] Migrar de `child_process` para `cardano-serialization-lib`
- [ ] Deploy em produção (mainnet)
