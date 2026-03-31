# Greentoken Cardano - Relatório de Desenvolvimento

**Data:** 30/03/2026 (atualizado)

---

## 1. Visão Geral do Projeto

O Greentoken Cardano é um sistema de **rastreamento de reciclagem de garrafas na blockchain Cardano** usando smart contracts Plutus V2. Cada garrafa passa por 5 estágios (`inserted → compacted → collected → atstation → shredded`), e o reciclador recebe **tokens Greentoken** como recompensa a cada transição.

O projeto é composto por três camadas:

| Camada | Tecnologia | Status |
|--------|-----------|--------|
| **On-chain** (smart contract) | Haskell / Plutus V2 | Concluída |
| **Off-chain** (scripts manuais) | Bash + cardano-cli | Concluída |
| **Backend** (API + banco de dados) | Node.js + TypeScript + PostgreSQL | Implementada, pendente testes |

---

## 2. O que foi Desenvolvido

### 2.1 Camada On-Chain

O contrato Plutus V2 (`onchain/src/Greentoken/BottleValidator.hs`) valida as transições de estágio de cada garrafa. Ele verifica que:
- A transição é válida (exemplo: `inserted → compacted` é permitido, mas `inserted → shredded` não)
- O datum contém o `pubkey_hash` do dono, o `bottle_id` e o estágio atual

O contrato compilado está em `assets/bottle-validator.plutus`.

### 2.2 Scripts Bash Off-Chain

#### Scripts de configuração inicial (setup)

Executados uma única vez para preparar o ambiente:

| Script | Função |
|--------|--------|
| `setup-wallet.sh` | Gera chaves do operador (`payment.vkey/skey/addr`) e endereço do script Plutus (`bottle.addr`) |
| `setup-policy.sh` | Gera chaves da minting policy (`policy.vkey/skey`), o native script (`policy.script`) e computa o `policyID` |

Ambos verificam se as chaves já existem para evitar sobrescrita acidental.

#### Scripts de operação

| Script | Função |
|--------|--------|
| `create-user.sh` | Gera par de chaves (vkey/skey), endereço testnet e **pubkey hash** (`.pkh`) para um novo usuário |
| `create-bottle.sh` | Cria uma garrafa no contrato, gera datums para todos os 5 estágios, faz mint de 10 Greentoken. Exibe o **TX_HASH** e o UTxO da garrafa |
| `advance-stage.sh` | Avança a garrafa para o próximo estágio, distribui recompensa em Greentoken. Exibe o **TX_HASH** e o novo UTxO |

#### Scripts de consulta

| Script | Função |
|--------|--------|
| `query-bottle.sh` | Consulta UTxOs no endereço do script Plutus. Aceita filtro por `TX_HASH`. Necessário para obter o `BOTTLE_TX_IN` usado no `advance-stage.sh` |
| `query-balance.sh` | Verifica saldo do operador (sem argumento), de um usuário (`./query-balance.sh user1`) ou de qualquer endereço Cardano |

**Recompensas por estágio (conforme scripts on-chain):**

| Estágio | Greentoken |
|---------|-----------|
| inserted | 10 |
| compacted | 10 |
| collected | 5 |
| atstation | 10 |
| shredded | 20 |
| **Total por garrafa** | **55** |

### 2.3 Banco de Dados PostgreSQL

Schema completo com 7 tabelas projetado para espelhar o estado on-chain e gerenciar a logística off-chain:

- **`users`**: recicladores e donos de pontos de coleta
- **`containers`**: pontos físicos de coleta com controle de volume/status
- **`trucks`**: frota de caminhões de coleta
- **`routes`** / **`route_stops`**: rotas inteligentes de coleta
- **`bottles`**: espelha os estágios do contrato Plutus, com `utxo_hash` + `utxo_index` para integrar com a blockchain
- **`blockchain_txs`**: log de auditoria de todas as transações submetidas
- **`rewards`**: registro de cada envio de Greentoken

Arquivos:
- `backend/db/schema.sql`: DDL completo com tabelas, índices, constraints e seed inicial
- `backend/greentoken_db_schema.html`: diagrama ERD visual (Mermaid.js)

### 2.4 Backend Node.js + TypeScript

Implementação completa da API que integra o banco de dados com a blockchain Cardano:

```
backend/src/
├── config.ts                         # Configuração via .env
├── validate.ts                       # Validação de inputs (anti command-injection)
├── db/
│   ├── pool.ts                       # Pool de conexão PostgreSQL
│   └── queries/
│       ├── users.ts                  # CRUD de usuários
│       ├── bottles.ts                # CRUD + gestão de estágio/UTxO
│       ├── containers.ts             # CRUD + volume/status
│       ├── blockchain-txs.ts         # CRUD + busca por pendentes
│       └── rewards.ts                # CRUD + total por usuário
├── services/
│   ├── cardano.service.ts            # Wrapper do cardano-cli (reimplementa os bash scripts)
│   ├── bottle.service.ts             # Orquestração: DB ↔ Cardano
│   └── container.service.ts          # Gestão de volume/status de containers
├── workers/
│   └── confirmation.worker.ts        # Polling de confirmação de txs on-chain (a cada 15s)
├── routes/
│   ├── users.routes.ts               # GET/POST /users
│   ├── bottles.routes.ts             # GET/POST /bottles, POST /bottles/:id/advance
│   └── containers.routes.ts          # GET/POST /containers, POST /:id/deposit
└── index.ts                           # Entry point Express (porta 3000)
```

**Endpoints da API:**

| Método | Rota | Descrição |
|--------|------|-----------|
| `GET` | `/health` | Health check (testa conexão com o banco) |
| `GET` | `/users` | Lista usuários (filtro: `?role=recycler\|owner`) |
| `GET` | `/users/:id` | Detalhe de um usuário |
| `POST` | `/users` | Cria novo usuário |
| `GET` | `/users/:id/rewards` | Histórico de recompensas + total Greentoken |
| `GET` | `/bottles` | Lista garrafas (filtros: `?user_id=`, `?stage=`, `?container_id=`) |
| `GET` | `/bottles/:id` | Detalhe + histórico completo (txs + rewards) |
| `POST` | `/bottles` | Cria garrafa (submete tx de mint na Cardano) |
| `POST` | `/bottles/:id/advance` | Avança estágio (submete tx de transição) |
| `GET` | `/containers` | Lista containers (filtros: `?status=`, `?owner_id=`) |
| `GET` | `/containers/:id` | Detalhe de um container |
| `POST` | `/containers` | Cria container |
| `POST` | `/containers/:id/deposit` | Registra volume depositado |
| `POST` | `/containers/:id/collected` | Marca container como coletado |
| `GET` | `/containers/status/full` | Lista containers cheios (prontos para rota) |

**Fluxo de integração:**

```
POST /bottles { bottle_id, user_id }
    │
    ├─ BottleService.create()
    │   ├─ CardanoService.createBottle()  ← chama cardano-cli via execFile
    │   ├─ INSERT bottles (utxo_hash = null)
    │   └─ INSERT blockchain_txs (status = pending)
    │
    │   ~~~ confirmation worker (a cada 15s) ~~~
    │
    ├─ Detecta UTxO on-chain
    │   ├─ UPDATE blockchain_txs → confirmed
    │   ├─ UPDATE bottles → utxo_hash = txHash#0
    │   └─ INSERT rewards (10 Greentoken)
    │
POST /bottles/:id/advance { stage: "compacted" }
    │
    ├─ BottleService.advance()
    │   ├─ Lê utxo_hash do banco  ← PONTO DE INTEGRAÇÃO
    │   ├─ CardanoService.advanceStage()
    │   └─ INSERT blockchain_txs (pending)
    │
    └─ Confirmation worker confirma e atualiza novamente
```

### 2.5 Ambiente Local Cardano

Nó Cardano configurado localmente:

- **cardano-node 10.5.4** + **cardano-cli 10.11.0.0** instalados em `~/.local/bin/`
- Nó sincronizado com a rede **Preprod** (testnet) via snapshot Mithril
- Configurações de rede em `~/cardano/preprod/`
- Socket em `~/cardano/preprod/node.socket`
- Network magic: `1`

---

## 3. Como Executar

### 3.1 Pré-requisitos

- PostgreSQL 16+ rodando localmente
- cardano-node e cardano-cli instalados (`~/.local/bin/`)
- Node.js 18+ e npm

### 3.2 Iniciar o Nó Cardano

```bash
# Iniciar
nohup ~/cardano/start-node.sh > ~/cardano/node.log 2>&1 &

# Verificar sincronização
cardano-cli conway query tip \
  --testnet-magic 1 \
  --socket-path ~/cardano/preprod/node.socket

# Parar
pkill -f cardano-node
```

Ou, se os aliases foram configurados:
```bash
cardano-start    # inicia em background
cardano-status   # verifica sync
cardano-stop     # para
```

### 3.3 Configurar o Banco de Dados

```bash
# Criar o banco (se ainda não foi criado)
sudo -u postgres psql -c "CREATE DATABASE greentoken_db;"

# Aplicar o schema
psql -U postgres -d greentoken_db -f backend/db/schema.sql
```

### 3.4 Iniciar o Backend

```bash
cd backend
cp .env.example .env  # editar com credenciais corretas
npm install
npm run dev           # inicia com ts-node na porta 3000
```

Variáveis do `.env`:
```env
DATABASE_URL=postgresql://postgres:suasenha@localhost:5432/greentoken_db
CARDANO_NODE_SOCKET_PATH=/home/exati/cardano/preprod/node.socket
CARDANO_NODE_MAGIC=1
PROJECT_ROOT=/home/exati/Desktop/unb/greentoken-cardano
PORT=3000
CONFIRMATION_POLL_MS=15000
```

### 3.5 Configuração inicial (uma única vez)

```bash
# Variáveis de ambiente necessárias
export CARDANO_NODE_SOCKET_PATH=~/cardano/preprod/node.socket
export CARDANO_NODE_MAGIC=1

# 1. Gerar chaves do operador e endereço do script
./setup-wallet.sh

# 2. Gerar chaves da minting policy
./setup-policy.sh

# 3. Obter tADA no faucet para o endereço exibido pelo setup-wallet.sh
https://docs.cardano.org/cardano-testnets/tools/faucet

# 4. Verificar recebimento
./query-balance.sh
```

### 3.6 Usar os Scripts Bash (modo manual)

```bash
# Criar usuário (gera chaves + pubkey hash)
./create-user.sh user3

# Criar garrafa (mint + depositar no contrato)
./create-bottle.sh bottle-1237 user1
# Saída inclui TX_HASH e UTxO (ex: abc123...#0)

# Consultar UTxOs da garrafa no script
./query-bottle.sh

# Avançar estágio (requer BOTTLE_TX_IN do UTxO atual)
./advance-stage.sh compacted bottle-1237 "$USER_ADDR" "$BOTTLE_TX_IN"
# Saída inclui TX_HASH e novo UTxO

# Verificar saldo de um usuário
./query-balance.sh user1
```

### 3.7 Testar a API

```bash
# Health check
curl http://localhost:3000/health

# Criar usuário
curl -X POST http://localhost:3000/users \
  -H "Content-Type: application/json" \
  -d '{"role":"recycler","name":"João","email":"joao@test.com","wallet_address":"addr_test1...","pubkey_hash":"abc123..."}'

# Criar garrafa
curl -X POST http://localhost:3000/bottles \
  -H "Content-Type: application/json" \
  -d '{"bottle_id":"bottle-001","user_id":"<UUID>"}'

# Avançar estágio
curl -X POST http://localhost:3000/bottles/<UUID>/advance \
  -H "Content-Type: application/json" \
  -d '{"stage":"compacted"}'

# Consultar recompensas
curl http://localhost:3000/users/<UUID>/rewards
```

---

## 4. Estrutura de Diretórios

### Projeto principal (`~/greentoken-cardano/`)

```
greentoken-cardano/
├── onchain/src/Greentoken/
│   └── BottleValidator.hs          # Smart contract Plutus V2
├── offchain/test/Greentoken/
│   └── BottleValidatorSpec.hs      # Testes do validador
├── app/
│   ├── Main.hs                     # CLI para exportar o contrato
│   └── WriteBottleValidator.hs     # Serialização do script
├── assets/
│   ├── bottle-validator.plutus     # Contrato compilado
│   ├── policy/                     # Minting policy do Greentoken
│   ├── redeemers/                  # Redeemers para cada transição
│   ├── wallet/                     # Endereços do script e operador
│   ├── users/                      # Chaves dos usuários de teste
│   └── protocol.json              # Parâmetros do protocolo
├── backend/
│   ├── db/schema.sql               # Schema PostgreSQL
│   ├── src/                        # Backend Node.js/TypeScript
│   ├── package.json
│   └── tsconfig.json
├── setup-wallet.sh                 # Setup: gerar chaves do operador + endereço do script
├── setup-policy.sh                 # Setup: gerar chaves da minting policy + policyID
├── create-user.sh                  # Operação: criar usuário (chaves + pubkey hash)
├── create-bottle.sh                # Operação: criar garrafa (mint + contrato)
├── advance-stage.sh                # Operação: avançar estágio de garrafa
├── query-bottle.sh                 # Consulta: UTxOs no endereço do script
├── query-balance.sh                # Consulta: saldo de qualquer endereço
├── SETUP-LOCAL.md                  # Guia de configuração do ambiente local
├── RELATORIO.md                    # Este relatório
├── plutus-greentoken.cabal         # Configuração Haskell
├── cabal.project                   # Dependências Haskell
└── README.md
```

### Nó Cardano (`~/cardano/`)

```
~/cardano/
├── start-node.sh                   # Script de inicialização do nó
├── mithril-client                  # Binário do Mithril (snapshot download)
├── mithril.tar.gz                  # Arquivo de instalação (pode remover)
├── mithril-aggregator              # Não necessário (pode remover)
├── mithril-relay                   # Não necessário (pode remover)
├── mithril-signer                  # Não necessário (pode remover)
├── lib*.a / lib*.so / lib*.rlib    # Bibliotecas Mithril (não necessárias)
└── preprod/
    ├── config.json                 # Configuração do nó
    ├── topology.json               # Peers da rede
    ├── *-genesis.json              # Arquivos genesis (Byron, Shelley, Alonzo, Conway)
    ├── node.socket                 # Socket de comunicação (gerado pelo nó)
    └── db/                         # Dados da blockchain (31 GB)
```

---

## 5. Próximos Passos

### Prioridade Alta

1. **Configurar PostgreSQL e rodar o schema**: criar o banco `greentoken_db`, aplicar `backend/db/schema.sql`, verificar que as tabelas foram criadas (ver seção 6 do `SETUP-LOCAL.md`)
2. **Gerar novas chaves para o ambiente local**: executar `./setup-wallet.sh` e `./setup-policy.sh` (ver seção 7 do `SETUP-LOCAL.md`)
3. **Obter tADA (ADA de teste)**: usar o faucet oficial para enviar ADA de teste ao endereço do operador (ver seção 8 do `SETUP-LOCAL.md`)
4. **Testar o backend end-to-end**: iniciar o nó Cardano + backend, criar um usuário via API, criar uma garrafa, verificar se a transação aparece on-chain e se o confirmation worker atualiza o banco (ver seção 10 do `SETUP-LOCAL.md`)

###

5. **Implementar lógica de rotas de caminhão**: quando um container fica `full`, criar automaticamente um `route_stop` vinculado à rota ativa do caminhão disponível
6. **Adicionar autenticação na API**: JWT ou API keys para proteger os endpoints
7. **Adicionar testes automatizados para o backend**: testes unitários para services e testes de integração para rotas
8. **Implementar frontend**: interface web para recicladores verem suas garrafas/recompensas e para owners monitorarem containers

###

9. **Migrar de `child_process` para `cardano-serialization-lib`**: construir transações diretamente em JavaScript sem depender do `cardano-cli` como subprocesso
10. **Adicionar suporte a múltiplos owners**: o schema já suporta, mas a lógica de negócio assume um único owner
11. **Integrar com hardware**: sensores nos containers para reportar volume automaticamente via API (`POST /containers/:id/deposit`)
12. **Deploy em produção (mainnet)**: migrar da Preprod para a mainnet da Cardano com chaves e políticas de produção
