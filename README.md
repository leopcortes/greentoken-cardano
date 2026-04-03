# Greentoken Cardano

Sistema de **rastreamento de reciclagem de garrafas na blockchain Cardano** usando smart contracts Plutus V2. Cada garrafa passa por 5 estágios (`inserted → compacted → collected → atstation → shredded`), e o reciclador recebe **tokens Greentoken** como recompensa a cada transição.

---

## Arquitetura

| Camada | Tecnologia | Descrição |
|--------|-----------|-----------|
| **On-chain** | Haskell / Plutus V2 | Smart contract que valida transições de estágio |
| **Off-chain** | Bash + cardano-cli | Scripts para operar diretamente na blockchain |
| **Backend** | Node.js + TypeScript + PostgreSQL | API REST que integra blockchain e banco de dados |
| **Frontend** | React + TypeScript + Vite + TailwindCSS | Dashboard web para gerenciar o sistema |

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
├── frontend/
│   ├── src/
│   │   ├── components/ui/            # Componentes shadcn (Radix + Tailwind)
│   │   ├── hooks/                    # Custom hooks (useSortable)
│   │   ├── lib/                      # Utilitários (truncateMiddle)
│   │   ├── pages/                    # Páginas (Bottles, Users, Containers, Routes)
│   │   ├── services/api.ts           # Cliente HTTP tipado para a API REST
│   │   ├── App.tsx                   # Layout principal com abas
│   │   └── index.css                 # Tema verde Tailwind + variáveis CSS
│   ├── vite.config.ts                # Vite + proxy /api → backend:3000
│   ├── tailwind.config.js            # Tailwind v3 + tokens shadcn
│   └── package.json
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
| `GET` | `/trucks` | Lista caminhões |
| `POST` | `/trucks` | Cadastra caminhão (`license_plate`) |
| `PATCH` | `/trucks/:id/status` | Atualiza status do caminhão |
| `GET` | `/routes` | Lista rotas de coleta |
| `POST` | `/routes` | Cria rota (truck_id + container_ids) |
| `GET` | `/routes/:id` | Detalhe da rota com paradas |
| `POST` | `/routes/stops/:stopId/collect` | Marca parada como coletada |

---

## Banco de Dados

Schema com 8 tabelas (`backend/db/schema.sql`):

- **`users`** - recicladores e donos de pontos de coleta
- **`containers`** - pontos físicos de coleta (volume, status)
- **`trucks`** - frota de caminhões
- **`routes`** / **`route_stops`** - rotas de coleta
- **`bottles`** - espelha os estágios do contrato Plutus (`utxo_hash` + `utxo_index`)
- **`blockchain_txs`** - log de auditoria de todas as transações
- **`rewards`** - registro de Greentoken enviados por estágio

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

# 5. Iniciar o frontend
cd frontend && npm install && npm run dev
# Acesse http://localhost:5173

# 6. Importar carteira e criar garrafa
scripts/import-user.sh user1 addr_test1q... "Nome" "email@test.com"
scripts/create-bottle.sh garrafa-001 user1
```

---

## Frontend (Dashboard)

Dashboard web construído com **React + TypeScript + Vite + TailwindCSS v3 + shadcn/ui** (componentes Radix UI).

### Funcionalidades

- **4 abas**: Garrafas, Usuários, Containers, Rotas/Caminhões
- **CRUD completo**: criar e visualizar registros de cada entidade via interface gráfica
- **Avanço de estágio**: botão para avançar garrafas no ciclo `inserted → shredded` (submete transação na blockchain via API)
- **Rotas de coleta**: selecionar caminhão + containers cheios, criar rota, coletar paradas individualmente
- **Recompensas**: dialog para visualizar recompensas Greentoken de cada usuário
- **UX aprimorada**: botões de copiar, colunas ordenáveis, truncamento inteligente de endereços, mensagens de erro formatadas

### Execução

```bash
cd frontend
npm install
npm run dev    # http://localhost:5173
```

O Vite faz proxy de `/api/*` para `http://localhost:3000` (backend). O backend precisa estar rodando.

---

## Próximos Passos

- [x] Frontend web para recicladores e owners
- [x] Implementar lógica de rotas de caminhão (CRUD de caminhões, criação de rotas, coleta de paradas)
- [ ] Adicionar autenticação na API (verificar se é necessário)
- [ ] Testes automatizados para o backend
- [ ] Migrar de `child_process` para `cardano-serialization-lib`
- [ ] Deploy em produção (mainnet)

Fluxo deve ser
1. É criada (inserida) uma garrafa associada a um container e usuário (garraga inserted)
2. Após n garrafas serem adicionadas a um container ele vai enchendo, ao ter 90% da capacidade ocupada ele pode ser coletado por um caminhão em uma rota (garrafa inserted -> colected)
3. Após a coleta as garrafas param de ser associadas ao container e ficam associadas ao caminhão (o container fica com ocupação vazia) (garrafa colected)
4. Após o caminhão terminar as coletas de sua rota ele leva as garrafas a uma estação de tratamento (ponto final da rota). Ao chegar na estação de tratamento as garrafas param de ser associadas ao caminhão e passam a ser associadas a estação de tratamento (garrafa colected -> atstation)
5. Na estação de tratamento existem n garrafas que podem ser trituradas (atstation -> shreded)

Obs:
- [ ] Garrafas que estiverem em um container não devem ter as ações de passar de estágio manualmente
- [ ] Ação no container para quando estiver pelo menos 90% cheio ele compactar
- [ ] Após todas as garrafas do container estarem compactadas, ao coletar um container, todas as garrafas dele devem passar para coletadas e o volume do container deve resetar
- [ ] após finalizar as coletas de uma rota, opção para "entregar" as garrafas do container em uma estação (isso avança as garrafas para o estágio atstation)
- [ ] Nova aba de estações de tratamento para ter opção de "triturar" as garrfas e passar para o próximo estágio
- [ ] Nova tabela de estações de tratamento no banco de dados