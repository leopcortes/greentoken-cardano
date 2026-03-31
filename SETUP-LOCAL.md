# Configuração do Ambiente Local — Greentoken Cardano

Guia passo a passo para configurar o ambiente de desenvolvimento local com cardano-node, cardano-cli, PostgreSQL e o backend Node.js na rede **Preprod** (testnet).

---

## Índice

1. [Pré-requisitos](#1-pré-requisitos)
2. [Instalar cardano-node e cardano-cli](#2-instalar-cardano-node-e-cardano-cli)
3. [Configurar a rede Preprod](#3-configurar-a-rede-preprod)
4. [Sincronizar via Mithril (recomendado)](#4-sincronizar-via-mithril-recomendado)
5. [Iniciar o nó Cardano](#5-iniciar-o-nó-cardano)
6. [Instalar e configurar PostgreSQL](#6-instalar-e-configurar-postgresql)
7. [Gerar chaves do operador e da policy](#7-gerar-chaves-do-operador-e-da-policy)
8. [Obter tADA (ADA de teste)](#8-obter-tada-ada-de-teste)
9. [Configurar e iniciar o backend](#9-configurar-e-iniciar-o-backend)
10. [Testar o fluxo completo](#10-testar-o-fluxo-completo)
11. [Comandos úteis](#11-comandos-úteis)
12. [Solução de problemas](#12-solução-de-problemas)

---

## 1. Pré-requisitos

| Software | Versão mínima | Verificar |
|----------|--------------|-----------|
| Linux (Ubuntu/Debian) | 22.04+ | `lsb_release -a` |
| Node.js | 18+ | `node --version` |
| npm | 9+ | `npm --version` |
| PostgreSQL | 14+ | `psql --version` |
| Git | 2.x | `git --version` |

Espaço em disco necessário: ~35 GB (31 GB para dados da blockchain Preprod + binários e configs).

---

## 2. Instalar cardano-node e cardano-cli

### 2.1 Baixar os binários pré-compilados

Acesse a página de releases do cardano-node:
**https://github.com/IntersectMBO/cardano-node/releases**

Baixe o arquivo `cardano-node-<versao>-linux.tar.gz` (exemplo: `cardano-node-10.5.4-linux.tar.gz`).

### 2.2 Extrair e instalar

```bash
# Extrair
cd ~/Downloads
tar -xzf cardano-node-*.tar.gz

# Os binários podem estar em subpastas (ex: bin/)
# Localize-os:
find . -name "cardano-node" -o -name "cardano-cli" | head -5

# Copiar para o PATH
mkdir -p ~/.local/bin
cp <caminho>/cardano-node ~/.local/bin/
cp <caminho>/cardano-cli ~/.local/bin/
chmod +x ~/.local/bin/cardano-node ~/.local/bin/cardano-cli
```

### 2.3 Adicionar ao PATH (se ainda não estiver)

Adicione ao `~/.bashrc` ou `~/.zshrc`:

```bash
export PATH="$HOME/.local/bin:$PATH"
```

Recarregue:
```bash
source ~/.bashrc
```

### 2.4 Verificar a instalação

```bash
cardano-node --version
# cardano-node 10.5.4 ...

cardano-cli --version
# cardano-cli 10.11.0.0 ...
```

---

## 3. Configurar a rede Preprod

### 3.1 Criar a estrutura de diretórios

```bash
mkdir -p ~/cardano/preprod/db
```

### 3.2 Baixar os arquivos de configuração

Os arquivos de configuração oficiais estão no repositório `iohk-nix`:

```bash
cd ~/cardano/preprod

# Config e topology
curl -sL https://raw.githubusercontent.com/IntersectMBO/cardano-world/master/docs/environments/preprod/config.json -o config.json
curl -sL https://raw.githubusercontent.com/IntersectMBO/cardano-world/master/docs/environments/preprod/topology.json -o topology.json

# Arquivos genesis (do iohk-nix — versões completas)
curl -sL https://raw.githubusercontent.com/input-output-hk/iohk-nix/master/cardano-lib/preprod/byron-genesis.json -o byron-genesis.json
curl -sL https://raw.githubusercontent.com/input-output-hk/iohk-nix/master/cardano-lib/preprod/shelley-genesis.json -o shelley-genesis.json
curl -sL https://raw.githubusercontent.com/input-output-hk/iohk-nix/master/cardano-lib/preprod/alonzo-genesis.json -o alonzo-genesis.json
curl -sL https://raw.githubusercontent.com/input-output-hk/iohk-nix/master/cardano-lib/preprod/conway-genesis.json -o conway-genesis.json
```

### 3.3 Corrigir o config.json

O `config.json` do cardano-world pode precisar de ajustes para versões recentes do cardano-node (10.x):

**a) Corrigir o ConwayGenesisHash:**

O hash no config.json precisa corresponder ao arquivo conway-genesis.json baixado. Se o nó reportar `GenesisHashMismatch`, extraia o hash correto da mensagem de erro e atualize o campo `ConwayGenesisHash` no `config.json`.

> Nota: Cardano usa **Blake2b-256**, não SHA-256. O hash correto aparece na mensagem de erro do nó.

**b) Adicionar TraceOptions (se ausente):**

Se o nó reclamar de `TraceOptions` faltando, adicione ao `config.json`:

```json
{
  "UseTraceDispatcher": false,
  "TraceOptions": {
    "ChainDB": { "severity": "Info" },
    "Net": { "severity": "Info" }
  }
}
```

### 3.4 Criar o script de inicialização

```bash
cat > ~/cardano/start-node.sh << 'EOF'
#!/usr/bin/env bash
cardano-node run \
  --config ~/cardano/preprod/config.json \
  --topology ~/cardano/preprod/topology.json \
  --database-path ~/cardano/preprod/db \
  --socket-path ~/cardano/preprod/node.socket \
  --port 3001
EOF

chmod +x ~/cardano/start-node.sh
```

---

## 4. Sincronizar via Mithril (recomendado)

Sincronizar a blockchain do zero leva **muitas horas**. O Mithril permite baixar um snapshot verificado em ~30 minutos.

### 4.1 Instalar o mithril-client

Acesse: **https://github.com/input-output-hk/mithril/releases**

Baixe o asset `mithril-<versao>-linux-x64.tar.gz` (NÃO o arquivo `mithril-client-linux-x64` que pode ser apenas um redirecionamento).

```bash
cd ~/cardano
tar -xzf ~/Downloads/mithril-*.tar.gz
# O binário mithril-client estará no diretório extraído
chmod +x mithril-client
```

### 4.2 Baixar o snapshot

```bash
cd ~/cardano

export AGGREGATOR_ENDPOINT="https://aggregator.release-preprod.api.mithril.network/aggregator"
export GENESIS_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey)

# Listar snapshots disponíveis
./mithril-client cardano-db snapshot list

# Baixar o mais recente (o primeiro da lista)
./mithril-client cardano-db download --download-dir ~/cardano/preprod/db latest
```

O download é de ~31 GB. Aguarde a conclusão e a verificação do certificado.

### 4.3 Mover os dados (se necessário)

O Mithril pode criar uma subpasta dentro de `db/`. Se existir `db/db/`, mova o conteúdo:

```bash
# Verifique a estrutura
ls ~/cardano/preprod/db/

# Se existir uma subpasta db/ dentro de db/:
mv ~/cardano/preprod/db/db/* ~/cardano/preprod/db/
rmdir ~/cardano/preprod/db/db
```

---

## 5. Iniciar o nó Cardano

### 5.1 Iniciar em background

```bash
nohup ~/cardano/start-node.sh > ~/cardano/node.log 2>&1 &
```

### 5.2 Verificar sincronização

```bash
cardano-cli conway query tip \
  --testnet-magic 1 \
  --socket-path ~/cardano/preprod/node.socket
```

Saída esperada quando sincronizado:
```json
{
  "epoch": 200,
  "syncProgress": "100.00",
  "slot": 82000000,
  ...
}
```

> **Nota:** Mesmo com `syncProgress: 100.00`, o nó continua rodando para acompanhar novos blocos. Isso é normal — ele precisa estar rodando sempre que você for submeter transações ou consultar a blockchain.

### 5.3 Parar o nó

```bash
pkill -f cardano-node
```

### 5.4 (Opcional) Aliases para conveniência

Adicione ao `~/.bashrc`:

```bash
alias cardano-start='nohup ~/cardano/start-node.sh > ~/cardano/node.log 2>&1 &'
alias cardano-stop='pkill -f cardano-node'
alias cardano-status='cardano-cli conway query tip --testnet-magic 1 --socket-path ~/cardano/preprod/node.socket'
alias cardano-log='tail -f ~/cardano/node.log'

export CARDANO_NODE_SOCKET_PATH=~/cardano/preprod/node.socket
export CARDANO_NODE_MAGIC=1
```

---

## 6. Instalar e configurar PostgreSQL

### 6.1 Instalar

```bash
sudo apt update
sudo apt install postgresql postgresql-contrib -y
```

### 6.2 Verificar que está rodando

```bash
sudo systemctl status postgresql
```

### 6.3 Definir a senha do usuário postgres

```bash
sudo -u postgres psql -c "ALTER USER postgres PASSWORD 'suasenha';"
```

Troque `suasenha` pela senha desejada. Essa mesma senha vai no `DATABASE_URL` do `.env`.

### 6.4 Criar o banco de dados

```bash
sudo -u postgres psql -c "CREATE DATABASE greentoken_db;"
```

### 6.5 Aplicar o schema

```bash
psql -U postgres -d greentoken_db -f backend/db/schema.sql
```

Se pedir senha, use a que definiu no passo 6.3.

### 6.6 Verificar as tabelas

```bash
psql -U postgres -d greentoken_db -c "\dt"
```

Deve listar: `users`, `containers`, `trucks`, `routes`, `route_stops`, `bottles`, `blockchain_txs`, `rewards`.

---

## 7. Gerar chaves do operador e da policy

Os scripts de transação precisam de chaves de assinatura (`.skey`) que não são commitadas no repositório por segurança.

### 7.1 Definir variáveis de ambiente

```bash
export CARDANO_NODE_SOCKET_PATH=~/cardano/preprod/node.socket
export CARDANO_NODE_MAGIC=1
```

### 7.2 Gerar chaves do operador

```bash
cd ~/Desktop/unb/greentoken-cardano
./setup-wallet.sh
```

Gera: `assets/wallet/payment.vkey`, `payment.skey`, `payment.addr`, `bottle.addr`

### 7.3 Gerar chaves da minting policy

```bash
./setup-policy.sh
```

Gera: `assets/policy/policy.vkey`, `policy.skey`, `policy.script`, `policyID`

> **ATENÇÃO:** Se você regenerar as chaves, o `policyID` muda e tokens antigos ficam incompatíveis. Guarde as chaves com segurança.

---

## 8. Obter tADA (ADA de teste)

O operador precisa de tADA para pagar as taxas de transação e depositar ADA nos UTxOs do script.

### 8.1 Verificar o endereço do operador

```bash
cat assets/wallet/payment.addr
```

### 8.2 Solicitar tADA no faucet

Acesse: **https://docs.cardano.org/cardano-testnets/tools/faucet**

1. Selecione a rede **Preprod**
2. Cole o endereço do operador
3. Solicite os fundos (normalmente 10.000 tADA)

### 8.3 Verificar o recebimento

Aguarde ~1 minuto e verifique:

```bash
./query-balance.sh
```

---

## 9. Configurar e iniciar o backend

### 9.1 Instalar dependências

```bash
cd backend
npm install
```

### 9.2 Configurar o .env

```bash
cp .env.example .env
```

Edite o `.env`:

```env
# PostgreSQL — ajuste a senha
DATABASE_URL=postgresql://postgres:suasenha@localhost:5432/greentoken_db

# Cardano Node
CARDANO_NODE_SOCKET_PATH=/home/<seu-usuario>/cardano/preprod/node.socket
CARDANO_NODE_MAGIC=1

# Raiz do projeto (onde fica a pasta assets/)
PROJECT_ROOT=/home/<seu-usuario>/Desktop/unb/greentoken-cardano

# Servidor
PORT=3000

# Polling de confirmação (ms)
CONFIRMATION_POLL_MS=15000
```

### 9.3 Iniciar o backend

```bash
npm run dev
```

### 9.4 Testar o health check

```bash
curl http://localhost:3000/health
```

Resposta esperada: `{"status":"ok"}`

---

## 10. Testar o fluxo completo

### 10.1 Criar um usuário via script

```bash
cd ~/Desktop/unb/greentoken-cardano
export CARDANO_NODE_SOCKET_PATH=~/cardano/preprod/node.socket
export CARDANO_NODE_MAGIC=1

./create-user.sh user-teste
```

Anote o endereço e o pubkey hash exibidos.

### 10.2 Enviar tADA ao usuário (necessário para receber tokens)

Solicite tADA para o endereço do usuário no faucet, ou transfira do operador.

### 10.3 Criar uma garrafa via API

```bash
# Primeiro, cadastre o usuário no banco
curl -X POST http://localhost:3000/users \
  -H "Content-Type: application/json" \
  -d '{
    "role": "recycler",
    "name": "Teste",
    "email": "teste@test.com",
    "wallet_address": "<ENDERECO_DO_USUARIO>",
    "pubkey_hash": "<PUBKEY_HASH>"
  }'

# Anote o "id" (UUID) retornado, depois:
curl -X POST http://localhost:3000/bottles \
  -H "Content-Type: application/json" \
  -d '{
    "bottle_id": "garrafa-001",
    "user_id": "<UUID_DO_USUARIO>"
  }'
```

### 10.4 Acompanhar a confirmação

O confirmation worker verifica a cada 15 segundos. Observe os logs do backend:

```
[worker] Verificando 1 tx(s) pendente(s)...
[worker] Tx abc123... confirmada — garrafa garrafa-001 → inserted
```

### 10.5 Avançar o estágio

```bash
curl -X POST http://localhost:3000/bottles/<UUID_DA_GARRAFA>/advance \
  -H "Content-Type: application/json" \
  -d '{"stage": "compacted"}'
```

### 10.6 Verificar recompensas

```bash
curl http://localhost:3000/users/<UUID_DO_USUARIO>/rewards
```

---

## 11. Comandos úteis

### Variáveis de ambiente (adicionar ao ~/.bashrc)

```bash
export CARDANO_NODE_SOCKET_PATH=~/cardano/preprod/node.socket
export CARDANO_NODE_MAGIC=1
```

### Scripts do projeto

| Script | Descrição | Uso |
|--------|-----------|-----|
| `setup-wallet.sh` | Gera chaves do operador | `./setup-wallet.sh` |
| `setup-policy.sh` | Gera chaves da minting policy | `./setup-policy.sh` |
| `create-user.sh` | Cria um novo usuário | `./create-user.sh <USER_ID>` |
| `create-bottle.sh` | Cria uma garrafa no contrato | `./create-bottle.sh <BOTTLE_ID> <USER_ID>` |
| `advance-stage.sh` | Avança estágio de uma garrafa | `./advance-stage.sh <STAGE> <BOTTLE_ID> <USER_ADDR> <TX_IN>` |
| `query-bottle.sh` | Consulta UTxOs no script | `./query-bottle.sh [TX_HASH]` |
| `query-balance.sh` | Consulta saldo | `./query-balance.sh [ADDR\|USER_ID]` |

### Cardano CLI

```bash
# Verificar sync do nó
cardano-cli conway query tip --testnet-magic 1 --socket-path ~/cardano/preprod/node.socket

# Consultar UTxOs de um endereço
cardano-cli conway query utxo --address <ADDR> --testnet-magic 1 --socket-path ~/cardano/preprod/node.socket

# Consultar parâmetros do protocolo
cardano-cli conway query protocol-parameters --testnet-magic 1 --socket-path ~/cardano/preprod/node.socket --out-file assets/pp.json
```

---

## 12. Solução de problemas

### Nó não inicia — `GenesisHashMismatch`

O hash no `config.json` não corresponde ao arquivo genesis baixado. O nó exibe uma mensagem como:

```
GenesisHashMismatch "hash_esperado" "hash_calculado"
```

Use o **primeiro** valor (`hash_esperado`) e atualize o campo correspondente no `config.json` (ex: `ConwayGenesisHash`).

### Nó não inicia — `NoDbMarkerAndNotEmpty`

O diretório `db/` contém dados corrompidos de uma tentativa anterior. Limpe e recomeçe:

```bash
rm -rf ~/cardano/preprod/db
mkdir ~/cardano/preprod/db
# Se usou Mithril, baixe o snapshot novamente
```

### Nó não inicia — `TraceOptions` / `UseTraceDispatcher`

Versões recentes do cardano-node (10.x) exigem estes campos no `config.json`. Adicione:

```json
"UseTraceDispatcher": false,
"TraceOptions": {
  "ChainDB": { "severity": "Info" },
  "Net": { "severity": "Info" }
}
```

### `cardano-cli: command not found`

Os binários não estão no PATH:

```bash
export PATH="$HOME/.local/bin:$PATH"
# Adicione ao ~/.bashrc para persistir
```

### `connect: does not exist (No such file or directory)` (socket)

O nó não está rodando ou o caminho do socket está errado:

```bash
# Verificar se o nó está rodando
ps aux | grep cardano-node

# Verificar se o socket existe
ls -la ~/cardano/preprod/node.socket

# Iniciar o nó
nohup ~/cardano/start-node.sh > ~/cardano/node.log 2>&1 &
```

### Backend não conecta no PostgreSQL — `password authentication failed`

Verifique a senha e o DATABASE_URL:

```bash
# Testar conexão diretamente
psql -U postgres -d greentoken_db -c "SELECT 1;"

# Se necessário, redefinir a senha
sudo -u postgres psql -c "ALTER USER postgres PASSWORD 'novasenha';"
```

### Mithril download — arquivo de 9 bytes

O link direto `mithril-client-linux-x64` pode ser um redirecionamento. Baixe o `.tar.gz` da página de releases:

```bash
# Verificar tamanho do arquivo
ls -la mithril-client
# Se < 1MB, o download falhou

# Baixar manualmente da página de releases
# https://github.com/input-output-hk/mithril/releases
```

### Transação falha — `UTxO balance insufficient`

O operador não tem tADA suficiente:

```bash
./query-balance.sh
# Se vazio ou < 5 ADA, solicite mais no faucet
```
