# Greentoken Cardano

Sistema de **rastreamento de reciclagem de garrafas na blockchain Cardano** usando smart contracts Plutus V3 escritos em Aiken. Cada garrafa passa por 5 estágios (`inserted -> compacted -> collected -> atstation -> shredded`), e o reciclador recebe **tokens Greentoken** como recompensa a cada transição.

### Recompensas por estágio

| Estágio | Greentoken | Descrição |
|---------|-----------|-----------|
| inserted | 10 | Garrafa inserida no container |
| compacted | 3 | Garrafas do container compactadas |
| collected | 7 | Container coletado pelo caminhão |
| atstation | 10 | Garrafas entregues na estação |
| shredded | 20 | Garrafas trituradas na estação |
| **Total** | **50** | Por garrafa, do início ao fim |

---

## Arquitetura

| Camada | Tecnologia | Descrição |
|--------|-----------|-----------|
| **On-chain** | Aiken / Plutus V3 | Validador da garrafa e minting policy do GreenToken |
| **Off-chain** | Bash + cardano-cli | Scripts para operar diretamente na blockchain |
| **Backend** | Node.js + TypeScript + PostgreSQL | API REST que integra blockchain e banco de dados |
| **Frontend** | React + TypeScript + Vite + TailwindCSS | Dashboard web para gerenciar o sistema |
| **Greenwallet** | Mesh SDK + Blockfrost + AES-256-GCM | Geração e custódia de wallets Cardano no servidor |

### Fluxo de integração com a blockchain

Cada operação no sistema segue o mesmo padrão assíncrono: o backend submete a transação Cardano imediatamente e retorna o `tx_hash` ao frontend, enquanto um **confirmation worker** em background aguarda a confirmação on-chain.

#### Componentes envolvidos

| Componente | Arquivo | Responsabilidade |
|---|---|---|
| **Frontend** | `frontend/src/pages/*.tsx` | Chama a API e faz polling até confirmar o UTxO |
| **API REST** | `backend/src/routes/*.routes.ts` | Recebe a requisição e aciona o serviço |
| **Bottle Service** | `backend/src/services/bottle.service.ts` | Orquestra blockchain + banco de dados |
| **Cardano Service** | `backend/src/services/cardano.service.ts` | Wrapper do `cardano-cli` - única camada que toca a blockchain |
| **Confirmation Worker** | `backend/src/workers/confirmation.worker.ts` | Detecta UTxOs confirmados e atualiza o banco |
| **Greenwallet** | `backend/src/greenwallet/crypto.ts` + `mesh.ts` | Geração de wallet BIP-39, criptografia AES-256-GCM e consulta de saldo via Blockfrost |
| **Validador da garrafa** | `aiken/validators/bottle.ak` | Smart contract Plutus V3 compilado em `assets/bottle-validator.plutus` |
| **Minting policy** | `aiken/validators/greentoken.ak` | Política de cunhagem Plutus V3 parametrizada pelo hash do validador, compilada em `assets/greentoken-policy.plutus` |

#### Detalhe do Cardano Service

O `cardano.service.ts` chama o `cardano-cli` diretamente para cada operação:

**Criar garrafa** - cria UTxO bloqueado no script + mint inicial:
```
1. queryUtxosJson(operatorAddr)    → localiza UTxO do operador com ≥5 ADA (usado também como collateral)
2. cardano-cli transaction build   → constrói tx: UTxO no script com inline datum {owner, bottleId, stage=inserted} + mint 10 Greentoken para o reciclador (com mint redeemer MintFor(Inserted))
3. cardano-cli transaction sign    → assina com operatorSkey
4. cardano-cli transaction submit  → envia ao nó Cardano (testnet)
5. retorna txHash imediatamente
```

**Avançar estágio** - consome UTxO antigo e cria novo:
```
1. Usa o utxo_hash + utxo_index da garrafa como tx-in no endereço do script (inline datum lido da própria UTxO)
2. Executa o validador Plutus (redeemer AdvanceTo(target))
3. Cria novo UTxO no script com inline datum atualizado (ex: stage=compacted)
4. Minta tokens de recompensa via minting policy Plutus, com mint redeemer MintFor(target)
```

#### Smart Contract (Plutus V3)

O validador on-chain aceita apenas transições sequenciais e exige continuidade do datum (mesmo `owner` e `bottleId` na saída):

```aiken
pub fn valid_transition(from: Stage, to: Stage) -> Bool {
  when from is {
    Inserted -> when to is { Compacted -> True; _ -> False }
    Compacted -> when to is { Collected -> True; _ -> False }
    Collected -> when to is { AtStation -> True; _ -> False }
    AtStation -> when to is { Shredded -> True; _ -> False }
    Shredded -> False
  }
}
```

A minting policy é parametrizada pelo hash do validador da garrafa e exige que cada cunhagem esteja acompanhada de uma saída ao endereço do script com o estágio correspondente, e que a quantidade emitida seja exatamente igual à recompensa daquele estágio. Se qualquer condição falhar, a transação é rejeitada pela rede Cardano.

#### Confirmation Worker

Roda a cada 5 segundos (configurável via `CONFIRMATION_POLL_MS`, padrão: `5000`):

1. Busca todas as `blockchain_txs` com `status='pending'`
2. Para cada tx: chama `queryUtxosJson(scriptAddr)` e verifica se `txHash#index` existe
3. Quando confirmado:
   - Atualiza `bottles.utxo_hash`, `bottles.utxo_index` e `bottles.current_stage`
   - Marca `blockchain_txs.status = 'confirmed'`
   - Cria registro em `rewards` com o valor do estágio

### Como cada operação usa a blockchain Cardano

| Operação | Transação on-chain | Detalhes |
|----------|-------------------|----------|
| **Criar garrafa** | Mint de NFT + 10 Greentoken | Cria um UTxO no endereço do script Plutus com datum `{user, bottleId, stage=inserted}`. Minta 10 Greentoken e envia à carteira do reciclador. |
| **Compactar** | Advance stage (por garrafa) | Consome o UTxO `inserted` do script, cria novo UTxO com datum `stage=compacted`. Minta 3 Greentoken para o reciclador. Requer redeemer de transição. |
| **Coletar** | Advance stage (por garrafa) | Consome UTxO `compacted`, cria UTxO `collected`. Minta 7 Greentoken. |
| **Entregar na estação** | Advance stage (por garrafa) | Consome UTxO `collected`, cria UTxO `atstation`. Minta 10 Greentoken. |
| **Triturar** | Advance stage (por garrafa) | Consome UTxO `atstation`, cria UTxO `shredded`. Minta 20 Greentoken. |

Cada transação é submetida via `cardano-cli`, assinada pelo operador + policy key, e confirmada pelo **confirmation worker** que verifica a presença do UTxO no endereço do script a cada 15 segundos.

---

## Estrutura do Projeto

```
greentoken-cardano/
|-- aiken/
|   |-- aiken.toml                    # Manifest Aiken (Plutus V3, stdlib v3.1.0)
|   |-- lib/greentoken/types.ak       # Tipos compartilhados (Stage, BottleDatum, BottleAction, MintAction)
|   |-- validators/bottle.ak          # Validador Plutus V3 da garrafa + testes inline
|   |-- validators/greentoken.ak      # Minting policy Plutus V3 do GreenToken
|-- assets/
|   |-- bottle-validator.plutus       # Validador da garrafa compilado (gerado por scripts/build-contracts.sh)
|   |-- greentoken-policy.plutus      # Minting policy compilada (gerada por scripts/build-contracts.sh)
|   |-- policy/policyID               # Policy ID do GreenToken (gerado por scripts/build-contracts.sh)
|   |-- redeemers/                    # Redeemers para o validador (advance) e para a minting policy (mint-<stage>)
|   |-- wallet/                       # Endereço do operador (payment.addr) e do script (bottle.addr)
|   |-- users/                        # Endereços e chaves dos usuários
|-- frontend/
|   |-- src/
|   |   |-- components/               # Componentes compartilhados (Bottle SVG, ui/ shadcn)
|   |   |-- hooks/                    # Custom hooks (useSortable)
|   |   |-- lib/                      # Utilitários (helpers, labels, types, sounds)
|   |   |-- pages/
|   |   |   |-- GreenStation/         # Tela do container (rota /): TopBar, CurrentBottle, CurrentContainer, CurrentWallet, Inventory, PipelinePanel + StationContext (drag/drop, polling, pipeline)
|   |   |   |-- Dashboard/            # Painel admin (rota /dashboard): Bottles, Users, Containers, Routes, Stations, Greenwallets, UserCreatedSeed
|   |   |-- services/api.ts           # Cliente HTTP tipado para a API REST
|   |   |-- App.tsx                   # Roteamento (Station em /, Dashboard em /dashboard)
|   |   |-- index.css                 # Tema verde Tailwind + variáveis CSS
|   |-- vite.config.ts                # Vite + proxy /api -> backend:3000
|   |-- tailwind.config.js            # Tailwind v3 + tokens shadcn
|   |-- package.json
|-- backend/
|   |-- db/schema.sql                 # Schema PostgreSQL (DDL + seed)
|   |-- db/migrations/                # Migrações incrementais (001: volume precision, 002: greenwallet, 003: rewards unique)
|   |-- src/
|   |   |-- services/                 # Lógica de negócio (bottle, container, cardano)
|   |   |-- routes/                   # Endpoints Express
|   |   |-- db/queries/               # Queries PostgreSQL tipadas
|   |   |-- workers/                  # Confirmation worker (polling blockchain)
|   |   |-- greenwallet/              # Geração de wallet BIP-39 (crypto.ts + mesh.ts)
|   |-- .env.example                  # Template de variáveis de ambiente
|   |-- package.json
|   |-- tsconfig.json
|-- scripts/
|   |-- _db-helper.sh                 # Helper: integração scripts <-> PostgreSQL
|   |-- build-contracts.sh            # Compila os contratos Aiken e exporta .plutus, bottle.addr e policyID
|   |-- setup-wallet.sh               # Setup: chaves do operador
|   |-- setup-policy.sh               # Compatibilidade: redireciona para build-contracts.sh
|   |-- split-utxos.sh                # Fragmenta UTXO do operador para operações batch
|   |-- query-balance.sh              # Consulta saldo de qualquer endereço
|   |-- get-pubkey-hash.sh            # Helper: gerar pubkey hash a partir do addr da wallet
|-- SETUP-LOCAL-MAC.md / SETUP-LOCAL-LINUX.md   # Guias completos de configuração local
|-- SETUP-CARDANO-LOCAL.md            # Guia de instalação do cardano-node
```

---

## Scripts

### Configuração inicial (executar uma vez)

| Script | Função | Uso |
|--------|--------|-----|
| `setup-wallet.sh` | Gera chaves do operador (`payment.{vkey,skey,addr}`) | `scripts/setup-wallet.sh` |
| `build-contracts.sh` | Compila os contratos Aiken e exporta `.plutus`, `bottle.addr` e `policyID` | `scripts/build-contracts.sh` |

### Operação

| Script | Função | Uso |
|--------|--------|-----|
| `split-utxos.sh` | Fragmenta o UTXO do operador em N UTXOs menores | `scripts/split-utxos.sh [N]` (padrão: 10) |
| `query-balance.sh` | Saldo de qualquer endereço | `scripts/query-balance.sh [ADDR\|USER_ID]` |

---

## API REST

O backend roda na porta 3000 e expõe os seguintes endpoints:

| Método | Rota | Descrição |
|--------|------|-----------|
| `GET` | `/health` | Health check |
| **Usuários** | | |
| `GET` | `/users` | Lista usuários (`?role=recycler\|owner`) |
| `GET` | `/users/:id` | Detalhe de um usuário |
| `POST` | `/users` | Cria usuário |
| `GET` | `/users/:id/rewards` | Recompensas + total Greentoken |
| **Garrafas** | | |
| `GET` | `/bottles` | Lista garrafas (`?user_id=`, `?stage=`, `?container_id=`, `?route_id=`, `?station_id=`) |
| `GET` | `/bottles/next-number` | Próximo número disponível para garrafa |
| `GET` | `/bottles/:id` | Detalhe + histórico (txs + rewards) |
| `POST` | `/bottles` | Cria garrafa (blockchain + banco) |
| **Containers** | | |
| `GET` | `/containers` | Lista containers (`?status=all\|active\|full\|compacted\|in_route`, `?owner_id=`) |
| `POST` | `/containers` | Cria container |
| `POST` | `/containers/:id/compact` | Compacta garrafas inserted (>= 90% cheio) |
| `POST` | `/containers/:id/collected` | Marca como coletado (esvaziado) |
| **Caminhões** | | |
| `GET` | `/trucks` | Lista caminhões |
| `POST` | `/trucks` | Cadastra caminhão (`license_plate`) |
| **Rotas** | | |
| `GET` | `/routes` | Lista rotas de coleta |
| `POST` | `/routes` | Cria rota (`truck_id` + `container_ids` + `station_id`) |
| `GET` | `/routes/:id` | Detalhe da rota com paradas |
| `POST` | `/routes/stops/:stopId/collect` | Coleta parada (compacted -> collected) |
| `POST` | `/routes/:id/deliver` | Entrega garrafas na estação (collected -> atstation) |
| **Estações** | | |
| `GET` | `/stations` | Lista estações de tratamento |
| `POST` | `/stations` | Cria estação |
| `GET` | `/stations/:id/bottles` | Lista garrafas na estação |
| `POST` | `/stations/:id/shred` | Tritura garrafas atstation (atstation -> shredded) |

---

## Banco de Dados

Schema com 8 tabelas (`backend/db/schema.sql`):

- **`users`** - recicladores e donos de pontos de coleta; colunas `mnemonic_ciphertext/iv/auth_tag/encryption_key_version` armazenam a wallet BIP-39 criptografada (greenwallet)
- **`containers`** - pontos físicos de coleta (volume, status: active/ready_for_collection/in_route/maintenance)
- **`trucks`** - frota de caminhões (status: available/on_route/maintenance)
- **`routes`** / **`route_stops`** - rotas de coleta com paradas em containers
- **`bottles`** - espelha os estágios do contrato Plutus (`utxo_hash` + `utxo_index`)
- **`blockchain_txs`** - log de auditoria de todas as transações submetidas
- **`rewards`** - registro de Greentoken creditados por estágio; constraint UNIQUE(bottle_id, stage) evita duplicatas
- **`stations`** - estações de tratamento de resíduos

> Migrações incrementais em `backend/db/migrations/` (aplicar após o schema inicial em ambientes legados).

---

## Quick Start

Consulte o [SETUP-LOCAL.md](SETUP-LOCAL.md) para o guia completo. Resumo rápido:

```bash
# 1. Configurar PostgreSQL
sudo -u postgres psql -c "CREATE DATABASE greentoken_db;"
psql -U postgres -d greentoken_db -f backend/db/schema.sql

# 2. Gerar chaves do operador (wallet owner) e compilar os contratos
export CARDANO_NODE_SOCKET_PATH=~/cardano/preprod/node.socket
export CARDANO_NODE_MAGIC=1
scripts/setup-wallet.sh
scripts/build-contracts.sh

# 3. Financiar wallet do operador com tADA (via faucet testnet Preprod)
cat assets/wallet/payment.addr
# Cole o endereço no faucet: https://docs.cardano.org/cardano-testnets/tools/faucet

# 3.1. Fragmentar UTXOs do operador (necessário para operações batch)
scripts/split-utxos.sh 100
# Aguarde ~20s e verifique: scripts/query-balance.sh

# 4. Iniciar nó e backend
cardano-start
cd backend && cp .env.example .env
# Preencha BLOCKFROST_API_KEY (https://blockfrost.io) e WALLET_ENCRYPTION_KEY (openssl rand -base64 32)
npm install && npm run dev

# 5. Iniciar o frontend
cd frontend && npm install && npm run dev
# Acesse http://localhost:5173

# 6. Criar usuários no frontend:
#    - Owner: usar endereço de assets/wallet/payment.addr
#      scripts/get-pubkey-hash.sh $(cat assets/wallet/payment.addr)
#    - Recycler: criar wallet via Lace (extensão Chrome, rede Preprod)
#      scripts/get-pubkey-hash.sh <ENDERECO_LACE>
#    Recyclers NÃO precisam de tADA (transações financiadas pelo owner)
```

---

## Frontend

Aplicação web construída com **React + TypeScript + Vite + TailwindCSS v3 + shadcn/ui** (componentes Radix UI). Possui duas telas principais:

| Rota | Tela | Função |
|------|------|--------|
| `/` | **Greentoken Station** | Simulador interativo do container físico (reciclador) |
| `/dashboard` | **Dashboard** | Painel CRUD para operadores (owners) |

### Greentoken Station

Simula a tela frontal do container Greentoken, visando o **reciclador**. Layout em 3 colunas:

- **Esquerda** - `CurrentBottlePage` (etapas da garrafa atual: Validada IA → Inserida → Compactada) + `CurrentWalletPage` (saldo Greentoken, endereço Cardano truncado, tier Bronze/Prata/Ouro, últimas tx hashes)
- **Centro** - `CurrentContainerPage` com SVG da lixeira (tampa animada, scan da IA, garrafas compactadas empilhadas, barra de volume com marker em 90%)
- **Direita** - `InventoryPage` (20 itens fixos: garrafas PET/HDPE válidas + lata e vidro como itens inválidos)

**Fluxo do drop:**
1. Reciclador arrasta uma garrafa do inventário (direita) para o container (centro) - tampa abre no hover
2. IA do container scaneia (linha varrendo, ~900ms) → aceita PET/HDPE ou rejeita lata/vidro com shake vermelho + toast de erro
3. Aceitação dispara `POST /bottles` no backend; o estado fica em "validating" até a confirmação on-chain
4. Pipeline `inserted → compacted` é alimentado pelo polling de `GET /bottles/:id` (5s); cada confirmação dispara confete de tokens voando do container até a wallet, count-up no saldo e linha nova no log de transações
5. Inventário é reposto automaticamente após cada drop (aceito ou rejeitado)

**Topbar:** chip de "garrafas processadas hoje" calculado a partir das recompensas do dia (stage=`inserted`), chip "Confirmando Xs" durante o polling on-chain, e link para o dashboard.

Cada operação chama os mesmos endpoints REST do dashboard (`POST /bottles`, `GET /bottles/:id`, `GET /users/:id/rewards`, `GET /containers`).

### Dashboard

Painel administrativo com 6 abas: Usuários, Garrafas, Containers, Rotas/Caminhões, Estações de Tratamento, Greenwallets. Recursos:

- **CRUD completo**: criar e visualizar registros de cada entidade via interface gráfica
- **Greenwallets**: criar wallets BIP-39 custodiadas no servidor, visualizar seed phrase, exportar endereço e pubkey hash
- **Bloqueios de fluxo**: botões desabilitados conforme regras de negócio (ex: não compactar container < 90%, não entregar na estação sem coletar todas as paradas)
- **Cooldown de blockchain**: após criar uma garrafa, o botão fica desabilitado até a transação ser confirmada on-chain (polling automático a cada 5s)
- **Rotas de coleta**: selecionar caminhão + containers compactados + estação de destino, coletar paradas e entregar na estação
- **Recompensas**: dialog para visualizar recompensas Greentoken de cada usuário
- **UX**: botões de copiar, colunas ordenáveis, truncamento inteligente de endereços, tooltips explicativos, mensagens de erro formatadas

### Execução

```bash
cd frontend
npm install
npm run dev    # http://localhost:5173
```

O Vite faz proxy de `/api/*` para `http://localhost:3000` (backend). O backend precisa estar rodando.

---

## Configuração de Wallets

Antes de usar o sistema, é necessário configurar as wallets Cardano para cada tipo de usuário:

### Wallet do Owner (operador)

A wallet do owner é criada via script e é responsável por **financiar todas as transações** on-chain (mint de NFTs, transições de estágio, recompensas). Sem ela, nenhuma operação na blockchain funciona.

1. Gere a wallet com `scripts/setup-wallet.sh` (cria chaves em `assets/wallet/`)
2. Financie a wallet com tADA via [faucet da testnet Preprod](https://docs.cardano.org/cardano-testnets/tools/faucet)
3. Com o endereço (`payment.addr`) e o pubkey hash (`scripts/get-pubkey-hash.sh`), crie o usuário **owner** no frontend

### Wallet dos Recyclers (recicladores)

As wallets dos recicladores servem para **identificar o usuário** e receber recompensas Greentoken. As transações são financiadas pela wallet do owner, portanto **recicladores não precisam de tADA**.

1. **Recomendado:** Crie a wallet pela extensão [Lace Wallet](https://www.lace.io/) no Chrome (rede Preprod)
2. **Alternativa:** Crie via `cardano-cli` (ver [SETUP-LOCAL.md](SETUP-LOCAL.md#75-criar-wallets-dos-recicladores-recyclers))
3. Com o endereço da wallet e o pubkey hash (`scripts/get-pubkey-hash.sh <ENDERECO>`), crie o usuário **recycler** no frontend

> Consulte o [SETUP-LOCAL.md](SETUP-LOCAL.md#7-configurar-wallets-e-chaves) para instruções detalhadas passo a passo.

---

## Fluxo de Utilização

O sistema segue um fluxo sequencial com bloqueios para evitar que etapas sejam puladas:

```
1. Configurar Wallets (owner via script + recycler via Lace/script)
         |
2. Criar Usuários no frontend (owner e recycler, com wallet address + pubkey hash)
         |
3. Criar Container (associado a um proprietário)
         |
4. Inserir Garrafa -> associada a um container e um usuário
   |  (garrafa: inserted | recompensa: 10 Greentoken)
   |  [botão bloqueado até confirmação on-chain da garrafa anterior]
         |
5. Container >= 90% -> botão "Compactar" habilitado
   |  (garrafa: inserted -> compacted | recompensa: 3 Greentoken)
   |  [container muda para status "compactado" após compactação]
         |
6. Criar Rota de Coleta
   |  (selecionar caminhão disponível + containers compactados + estação de destino)
         |
7. Coletar Paradas da Rota (uma a uma)
   |  (garrafa: compacted -> collected | recompensa: 7 Greentoken)
   |  [garrafas saem do container e ficam associadas ao caminhão]
   |  [container volta a status "ativo" com volume zerado]
         |
8. Entregar na Estação (só após TODAS as paradas coletadas)
   |  (garrafa: collected -> atstation | recompensa: 10 Greentoken)
   |  [garrafas saem do caminhão e ficam associadas à estação]
         |
9. Triturar na Estação
   (garrafa: atstation -> shredded | recompensa: 20 Greentoken)
```

**Total de recompensa por garrafa: 50 Greentoken**

---

## Próximos Passos

- [x] Frontend web para recicladores e owners
- [x] Tela Greentoken Station (simulação interativa do container físico, com drag-drop, IA de validação e pipeline on-chain)
- [x] Implementar lógica de rotas de caminhão (CRUD de caminhões, criação de rotas, coleta de paradas)
- [x] Estações de tratamento e trituração
- [x] Bloqueios de fluxo no frontend (evitar etapas fora de ordem)
- [x] Cooldown de blockchain na criação de garrafas
- [x] Correção de recompensas em operações batch multi-usuário
- [x] Lidar com delays de operações na blockchain (validação de UTXOs confirmados + fragmentação de UTXOs)
- [x] Greenwallet: geração de wallets BIP-39 custodiadas no servidor com criptografia AES-256-GCM
- [ ] Adicionar autenticação na API
- [ ] Testes automatizados para o backend
- [ ] Migrar de `child_process` para `cardano-serialization-lib`
- [ ] Deploy em produção (mainnet)