# Configuração do Ambiente Local - Greentoken Cardano (macOS)

Guia passo a passo para configurar o ambiente de desenvolvimento local com cardano-node, cardano-cli, PostgreSQL, backend Node.js e frontend React na rede **Preprod** (testnet).

---

## Índice

1. [Pré-requisitos](#1-pré-requisitos)
2. [Instalar cardano-node e cardano-cli](#2-instalar-cardano-node-e-cardano-cli)
3. [Configurar a rede Preprod](#3-configurar-a-rede-preprod)
4. [Sincronizar via Mithril (recomendado)](#4-sincronizar-via-mithril-recomendado)
5. [Iniciar o nó Cardano](#5-iniciar-o-nó-cardano)
6. [Instalar e configurar PostgreSQL](#6-instalar-e-configurar-postgresql)
7. [Configurar e iniciar o backend](#7-configurar-e-iniciar-o-backend)
8. [Configurar e iniciar o frontend](#8-configurar-e-iniciar-o-frontend)
9. [Configurar wallets e chaves](#9-configurar-wallets-e-chaves)
10. [Testar o fluxo completo](#10-testar-o-fluxo-completo)
11. [Comandos úteis](#11-comandos-úteis)
12. [Solução de problemas](#12-solução-de-problemas)

---

## 1. Pré-requisitos

| Software | Versão mínima | Verificar |
|----------|--------------|-----------|
| macOS | 13 (Ventura)+ | `sw_vers` |
| Homebrew | qualquer | `brew --version` |
| Node.js | 18+ | `node --version` |
| npm | 9+ | `npm --version` |
| PostgreSQL | 14+ | `psql --version` |
| Git | 2.x | `git --version` |

Espaço em disco necessário: ~20 GB (17 GB para dados da blockchain Preprod + binários e configs).

### Instalar o Homebrew (caso não tenha)

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

Após a instalação, siga as instruções exibidas no terminal para adicionar o Homebrew ao PATH (especialmente necessário em Macs com Apple Silicon).

---

## 2. Instalar cardano-node e cardano-cli

### 2.1 Baixar os binários pré-compilados

Acesse a página de releases do cardano-node:
**https://github.com/IntersectMBO/cardano-node/releases**

Baixe o arquivo `cardano-node-<versao>-macos.tar.gz` (exemplo: `cardano-node-10.5.4-macos.tar.gz`).

> **Qual arquivo baixar?** Use `uname -m` no terminal:
> - Resultado `arm64` → baixe `cardano-node-<versao>-macos-arm64.tar.gz` (Apple Silicon M1 ou posterior)
> - Resultado `x86_64` → baixe `cardano-node-<versao>-macos-amd64.tar.gz` (Intel)

### 2.2 Extrair e instalar

```bash
mkdir -p /tmp/cardano-extract
tar -xzf ~/Downloads/cardano-node-*-macos-arm64.tar.gz -C /tmp/cardano-extract
```

O conteúdo estará em `/tmp/cardano-extract/bin/` com os executáveis **e** as bibliotecas `.dylib` juntas. Copie **tudo**:

```bash
mkdir -p ~/.local/bin
cp /tmp/cardano-extract/bin/cardano-node ~/.local/bin/
cp /tmp/cardano-extract/bin/cardano-cli ~/.local/bin/
cp /tmp/cardano-extract/bin/*.dylib ~/.local/bin/
cp /tmp/cardano-extract/bin/*.so ~/.local/bin/ 2>/dev/null || true
```

> **Importante:** os binários usam `@executable_path/` para localizar as `.dylib`, ou seja, elas **precisam estar na mesma pasta** que os executáveis. Copiar só `cardano-cli` e `cardano-node` resulta em `zsh: killed`.

### 2.3 Re-assinar os binários (obrigatório no macOS)

O macOS invalida a assinatura de código ao copiar binários baixados da internet. Re-assine com assinatura local:

```bash
codesign --force --deep --sign - ~/.local/bin/cardano-cli
codesign --force --deep --sign - ~/.local/bin/cardano-node
```

### 2.4 Adicionar ao PATH (se ainda não estiver)

Adicione ao `~/.zshrc` (shell padrão no macOS):

```bash
export PATH="$HOME/.local/bin:$PATH"
```

Recarregue:
```bash
source ~/.zshrc
```

### 2.5 Verificar a instalação (após recarregar o shell)

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

```bash
cd ~/cardano/preprod

curl -sL https://raw.githubusercontent.com/IntersectMBO/cardano-world/master/docs/environments/preprod/config.json -o config.json
curl -sL https://raw.githubusercontent.com/IntersectMBO/cardano-world/master/docs/environments/preprod/topology.json -o topology.json
curl -sL https://raw.githubusercontent.com/IntersectMBO/cardano-world/master/docs/environments/preprod/byron-genesis.json -o byron-genesis.json
curl -sL https://raw.githubusercontent.com/IntersectMBO/cardano-world/master/docs/environments/preprod/shelley-genesis.json -o shelley-genesis.json
curl -sL https://raw.githubusercontent.com/IntersectMBO/cardano-world/master/docs/environments/preprod/alonzo-genesis.json -o alonzo-genesis.json
curl -sL https://book.play.dev.cardano.org/environments/preprod/conway-genesis.json -o conway-genesis.json
```

### 3.3 Criar o script de inicialização

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

- **Mac Intel (x86_64):** baixe `mithril-<versao>-macos-x64.tar.gz`
- **Apple Silicon (M1 ou posterior):** baixe `mithril-<versao>-macos-arm64.tar.gz`

```bash
cd ~/cardano
tar -xzf ~/Downloads/mithril-*.tar.gz
chmod +x mithril-client

# Remover o atributo de quarentena do Gatekeeper
xattr -d com.apple.quarantine mithril-client
```

### 4.2 Baixar o snapshot

```bash
cd ~/cardano

export AGGREGATOR_ENDPOINT="https://aggregator.release-preprod.api.mithril.network/aggregator"
export GENESIS_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey)

# Listar snapshots disponíveis
./mithril-client cardano-db snapshot list

# Baixar o mais recente
./mithril-client cardano-db download --download-dir ~/cardano/preprod/db latest
```

### 4.3 Mover os dados (se necessário)

O Mithril pode criar uma subpasta `db/` dentro de `db/`. Se existir, mova o conteúdo:

```bash
# Verifique a estrutura
ls ~/cardano/preprod/db/

# Se existir db/db/:
mv ~/cardano/preprod/db/db/* ~/cardano/preprod/db/
rmdir ~/cardano/preprod/db/db
```

---

## 5. Iniciar o nó Cardano

### 5.1 Definir aliases para conveniência

Adicione ao `~/.zshrc` antes de iniciar o nó pela primeira vez:

```bash
alias cardano-start='nohup ~/cardano/start-node.sh > ~/cardano/node.log 2>&1 & echo Nó iniciado'
alias cardano-end='pkill -f cardano-node && echo Nó parado'
alias cardano-report='cardano-cli conway query tip --testnet-magic 1 --socket-path ~/cardano/preprod/node.socket'
alias cardano-log='tail -f ~/cardano/node.log'

export CARDANO_NODE_SOCKET_PATH=~/cardano/preprod/node.socket
export CARDANO_NODE_MAGIC=1
```

Recarregue:
```bash
source ~/.zshrc
```

### 5.2 Iniciar o nó

```bash
cardano-start
```

### 5.3 Verificar e corrigir o config.json (se necessário)

Observe os logs por alguns segundos após iniciar:

```bash
cardano-log
```

#### Erro: `GenesisHashMismatch`

Se o nó encerrar com a seguinte mensagem:

```
GenesisHashMismatch "aaaa...1111" "bbbb...2222"
```

O nó está comparando o hash registrado no `config.json` com o hash real do arquivo `conway-genesis.json` no disco. O formato é:

```
GenesisHashMismatch "<hash que está no config.json>" "<hash real do arquivo em disco>"
```

**O segundo valor é o correto** - é o hash calculado do arquivo que você baixou. Atualize o campo `ConwayGenesisHash` no `config.json` com ele:

```bash
nano ~/cardano/preprod/config.json
```

Localize a linha com o hash antigo e substitua pelo segundo valor da mensagem de erro:

```json
"ConwayGenesisHash": "bbbb...2222"
```

Reinicie o nó após corrigir:

```bash
cardano-end
cardano-start
cardano-log
```

#### Erro: `TraceOptions` / `UseTraceDispatcher`

Se o nó reclamar de `TraceOptions` faltando, adicione ao `config.json`:

```json
"UseTraceDispatcher": false,
"TraceOptions": {
  "ChainDB": { "severity": "Info" },
  "Net": { "severity": "Info" }
}
```

Reinicie com `cardano-end && cardano-start` após qualquer alteração no `config.json`.

### 5.4 Verificar sincronização

Aguarde ~10 segundos após iniciar e consulte o progresso:

```bash
cardano-report
```

Saída esperada quando sincronizado:
```json
{
  "epoch": 280,
  "syncProgress": "100.00",
  "slot": 119000000
}
```

> **Nota:** O nó precisa estar rodando sempre que for submeter transações ou consultar a blockchain. Após o `cardano-start`, aguarde ~10 segundos antes de usar o `cardano-cli` (o socket demora para ficar disponível).

### 5.5 Parar o nó

```bash
cardano-end
```

---

## 6. Instalar e configurar PostgreSQL

### 6.1 Instalar via Homebrew

```bash
brew install postgresql@14
```

### 6.2 Iniciar o serviço

```bash
brew services start postgresql@14
```

### 6.3 Verificar que está rodando

```bash
brew services list | grep postgresql
```

O status deve aparecer como `started`.

### 6.4 Criar o usuário postgres e definir senha

No macOS com Homebrew, o superusuário padrão é o seu usuário do sistema (não `postgres`). Para manter compatibilidade com o restante do guia, crie o usuário `postgres`:

```bash
# Conecte como superusuário do sistema
psql postgres

# Dentro do psql:
CREATE USER postgres WITH SUPERUSER PASSWORD 'suasenha';
\q
```

Troque `suasenha` pela senha desejada. Essa mesma senha vai no `DATABASE_URL` do `.env`.

### 6.5 Criar o banco de dados

```bash
psql -U postgres -c "CREATE DATABASE greentoken_db;"
```

### 6.6 Aplicar o schema

```bash
psql -U postgres -d greentoken_db -f backend/db/schema.sql
```

Se pedir senha, use a que definiu no passo 6.4.

### 6.7 Verificar as tabelas

```bash
psql -U postgres -d greentoken_db -c "\dt"
```

Deve listar: `users`, `containers`, `trucks`, `routes`, `route_stops`, `bottles`, `blockchain_txs`, `rewards`.

---

## 7. Configurar e iniciar o backend

### 7.1 Instalar dependências

```bash
cd backend
npm install
```

### 7.2 Configurar o .env

```bash
cp .env.example .env
```

Edite o `.env`:

```env
# PostgreSQL - ajuste a senha
DATABASE_URL=postgresql://postgres:suasenha@localhost:5432/greentoken_db

# Cardano Node
CARDANO_NODE_SOCKET_PATH=/Users/<seu-usuario>/cardano/preprod/node.socket
CARDANO_NODE_MAGIC=1

# Raiz do projeto (onde fica a pasta assets/)
PROJECT_ROOT=/Users/<seu-usuario>/caminho/para/greentoken-cardano

# Servidor
PORT=3000

# Polling de confirmação (ms)
CONFIRMATION_POLL_MS=15000
```

> **Atenção:** No macOS, o diretório home é `/Users/<seu-usuario>` e não `/home/<seu-usuario>`.

### 7.3 Iniciar o backend

```bash
npm run dev
```

### 7.4 Testar o health check

```bash
curl http://localhost:3000/health
```

Resposta esperada: `{"status":"ok","db":"connected"}`

---

## 8. Configurar e iniciar o frontend

O frontend é um dashboard React que consome a API REST do backend.

### 8.1 Pré-requisitos do frontend

O frontend usa **Vite 5** + **TailwindCSS v3**, compatível com Node.js 18+. As mesmas dependências de Node.js do backend são suficientes.

### 8.2 Instalar dependências

```bash
cd frontend
npm install
```

### 8.3 Iniciar o servidor de desenvolvimento

```bash
npm run dev
```

O frontend estará disponível em **http://localhost:5173**.

### 8.4 Proxy para o backend

O Vite está configurado para redirecionar chamadas `/api/*` para `http://localhost:3000` (backend). Não é necessário configurar CORS - basta que o backend esteja rodando na porta 3000.

### 8.5 Funcionalidades do dashboard

| Aba | Funcionalidades |
|-----|----------------|
| **Usuários** | Listar, criar usuários (reciclador/proprietário), ver recompensas Greentoken |
| **Garrafas** | Listar garrafas com localização unificada (Container/Caminhão/Estação), criar garrafas (com cooldown de blockchain), ver UTXO |
| **Containers** | Listar, criar containers, barra de volume visual, compactar (>= 90%), status compactado |
| **Rotas** | Cadastrar caminhões, criar rotas com containers compactados, coletar paradas, entregar na estação |
| **Estações** | Listar, criar estações de tratamento, ver garrafas, triturar garrafas |

### 8.6 Stack técnica

- **React 18** + TypeScript
- **Vite 5** (bundler, compatível com Node 18)
- **TailwindCSS v3** + PostCSS (Tailwind v4 requer Node 20+)
- **shadcn/ui** (componentes escritos manualmente com Radix UI para compatibilidade com Tailwind v3)
- **lucide-react** (ícones)

---

## 9. Configurar wallets e chaves

O sistema usa dois tipos de wallet: a **wallet do operador (owner)** que financia todas as transações on-chain, e as **wallets dos recicladores (recyclers)** que apenas identificam os usuários e recebem recompensas Greentoken.

> ### Atenção! Configurando em um dispositivo secundário?
>
> O `schema.sql` já insere automaticamente os dados iniciais ao ser aplicado (passo 6.6):
> - **Usuários:** owner padrão + 2 recicladores (com wallet address e pubkey_hash já definidos)
> - **Containers:** Ponto de Coleta FT UnB (10L) e BCE UnB (5L)
> - **Estações:** Central de Reciclagem e Complexo Integrado de Reciclagem
> - **Caminhões:** PBX-6480, XYZ-9876 e ABC-1234
>
> Por isso, **pule os passos 9.5, 9.6 e 9.7** - os usuários e demais dados já foram criados pelo seed.
>
> **Não execute** `scripts/setup-wallet.sh` (9.2) nem `scripts/setup-policy.sh` (9.8). Recriar as chaves geraria um endereço de wallet e um `policyID` diferentes, tornando os tokens e UTXOs existentes na blockchain inacessíveis.
>
> Em vez disso, **copie manualmente** do dispositivo original os arquivos abaixo (excluídos do repositório por segurança):
> ```
> assets/wallet/payment.skey
> assets/wallet/payment.vkey
> assets/policy/policy.skey
> assets/policy/policy.vkey
> ```
>
> Se não tiver acesso, solicite ao dono do repositório.
>
> Após copiar as chaves, execute apenas:
> - **9.1** - definir variáveis de ambiente
> - **9.3** - verifique o saldo com `scripts/query-balance.sh` (o tADA está on-chain, não no dispositivo)
> - **9.4** - fragmente UTXOs se necessário

### 9.1 Definir variáveis de ambiente

(Já deve estar configurado em `~/.zshrc` pelo passo 5.1)

```bash
export CARDANO_NODE_SOCKET_PATH=~/cardano/preprod/node.socket
export CARDANO_NODE_MAGIC=1
```

### 9.2 Criar a wallet do operador (owner)

A wallet do operador é usada para assinar e financiar **todas** as transações na blockchain (mint de NFTs, transições de estágio, recompensas). Ela deve ser criada via script:

```bash
scripts/setup-wallet.sh
```

Gera: `assets/wallet/payment.vkey`, `payment.skey`, `payment.addr`, `bottle.addr`

Anote o endereço exibido ao final (`payment.addr`), ele será usado no próximo passo para receber tADA e, depois, para criar o usuário owner no frontend.

### 9.3 Financiar a wallet do operador com tADA

A wallet do operador **precisa de tADA** para pagar as taxas de todas as transações on-chain. Sem fundos, nenhuma operação (criar garrafa, compactar, coletar, etc.) funcionará.

1. Copie o endereço do operador:
   ```bash
   cat assets/wallet/payment.addr
   ```
2. Acesse o faucet oficial da testnet Preprod:
   **https://docs.cardano.org/cardano-testnets/tools/faucet**
3. Selecione **Preprod**, cole o endereço e solicite os fundos (10.000 tADA)
4. Verifique o recebimento (pode levar ~1 minuto):
   ```bash
   scripts/query-balance.sh
   ```

### 9.4 Fragmentar UTXOs do operador (obrigatório para operações batch)

Operações que processam múltiplas garrafas (compactar, coletar, entregar) precisam de **1 UTXO do operador por garrafa**. O faucet envia tudo em um único UTXO, então é necessário fragmentá-lo:

```bash
scripts/split-utxos.sh 100
```

Isso cria 100 UTXOs de 10 ADA cada + 1 UTXO de troco. Aguarde ~20 segundos e verifique:

```bash
scripts/query-balance.sh
```

Deve listar 101 UTXOs. Sem essa etapa, operações batch processam apenas 1 garrafa por vez.

> **Dica:** Se durante o uso os UTXOs acabarem (muitas operações em sequência), execute o script novamente.

### 9.5 Criar o usuário owner no frontend

Com a wallet do operador criada e financiada:

1. Acesse o frontend em **http://localhost:5173** (backend precisa estar rodando)
2. Na aba **Usuários**, clique em **Novo Usuário**
3. Selecione o cargo **owner**
4. Preencha nome, email e cole o endereço da wallet do operador (`payment.addr`)

### 9.6 Criar wallets dos recicladores (recyclers)

As wallets dos recicladores servem apenas para **identificar o usuário** e receber recompensas Greentoken. As transações on-chain são financiadas pela wallet do operador, portanto **os recicladores não precisam de tADA** em suas wallets.

**Opção recomendada: Carteira Lace (extensão Chrome)**

1. Instale a extensão **Lace Wallet** no Chrome: https://www.lace.io/
2. Crie uma nova wallet e selecione a rede **Preprod**
3. Copie o endereço da wallet (formato `addr_test1...`)

**Opção alternativa: Via script com cardano-cli**

Se preferir, crie manualmente (ex: para testes automatizados):

```bash
# Crie um diretório para o reciclador
mkdir -p assets/users/recycler1

# Gere o par de chaves
cardano-cli address key-gen \
  --verification-key-file assets/users/recycler1/payment.vkey \
  --signing-key-file assets/users/recycler1/payment.skey

# Derive o endereço
cardano-cli address build \
  --payment-verification-key-file assets/users/recycler1/payment.vkey \
  --testnet-magic $CARDANO_NODE_MAGIC \
  --out-file assets/users/recycler1/payment.addr

cat assets/users/recycler1/payment.addr
```

### 9.7 Criar o usuário recycler no frontend

Com o endereço da wallet do reciclador em mãos:

1. Na aba **Usuários**, clique em **Novo Usuário**
2. Selecione o cargo **recycler**
3. Preencha nome, email e cole o endereço da wallet do reciclador
4. Para o campo `pubkey_hash`, gere-o a partir do endereço:
   ```bash
   scripts/get-pubkey-hash.sh <ENDERECO_DO_RECYCLER>
   ```

### 9.8 Gerar chaves da minting policy

```bash
scripts/setup-policy.sh
```

Gera: `assets/policy/policy.vkey`, `policy.skey`, `policy.script`, `policyID`

> **ATENÇÃO:** Se você regenerar as chaves, o `policyID` muda e tokens antigos ficam incompatíveis. Guarde as chaves com segurança.

---

## 10. Testar o fluxo completo

### 10.1 Via Dashboard (Front-end)

1. Acesse **http://localhost:5173** (com backend rodando)
2. Na aba **Usuários**, verifique que os usuários `owner` e `recycler` já foram criados (passos 9.5 e 9.7)
3. Na aba **Containers**, crie um container (associado ao owner) com capacidade pequena (ex: 1L) para facilitar testes
4. Na aba **Garrafas**, crie garrafas associadas ao usuário e container. Aguarde o botão "Nova Garrafa" desbloquear (confirmação on-chain ~15s) antes de criar a próxima
5. Na aba **Containers**, quando o container atingir >= 90%, clique em **Compactar** (o status muda para "Compactado")
6. Na aba **Estações**, crie uma estação de tratamento
7. Na aba **Rotas**, cadastre um caminhão, depois crie uma rota selecionando o caminhão + container compactado + estação de destino
8. Nos **Detalhes da rota**, colete cada parada individualmente
9. Após todas as paradas coletadas, clique em **Entregar Garrafas na Estação**
10. Na aba **Estações**, clique em **Ver Garrafas** e depois **Triturar Todas**
11. Na aba **Usuários**, clique em **Recompensas** para ver o total de Greentoken acumulado

### 10.2 Via API

#### 10.2.1 Criar um usuário

```bash
curl -X POST http://localhost:3000/users \
  -H "Content-Type: application/json" \
  -d '{
    "role": "recycler",
    "name": "Teste",
    "email": "teste@test.com",
    "wallet_address": "<ENDERECO>",
    "pubkey_hash": "<PUBKEY_HASH>"
  }'
```

#### 10.2.2 Criar uma garrafa

```bash
curl -X POST http://localhost:3000/bottles \
  -H "Content-Type: application/json" \
  -d '{"bottle_id": "garrafa-001", "user_id": "<UUID>"}'
```

#### 10.2.3 Acompanhar a confirmação

O confirmation worker verifica a cada 15 segundos. Observe os logs do backend:

```
[worker] Tx abc123... confirmada - garrafa garrafa-001 → inserted
```

Ou consulte via API:

```bash
curl http://localhost:3000/bottles/<UUID>
```

Quando confirmado, `utxo_hash` será preenchido e uma recompensa de 10 Greentoken será registrada.

#### 10.2.4 Avançar o estágio

```bash
curl -X POST http://localhost:3000/bottles/<UUID>/advance \
  -H "Content-Type: application/json" \
  -d '{"stage": "compacted"}'
```

#### 10.2.5 Verificar recompensas

```bash
curl http://localhost:3000/users/<UUID>/rewards
```

#### 10.2.6 Testar caminhões e rotas

```bash
# Cadastrar caminhão
curl -X POST http://localhost:3000/trucks \
  -H "Content-Type: application/json" \
  -d '{"license_plate": "GRN-0001"}'

# Criar rota de coleta (associa caminhão a containers cheios)
curl -X POST http://localhost:3000/routes \
  -H "Content-Type: application/json" \
  -d '{"truck_id": "<TRUCK_UUID>", "container_ids": ["<CONTAINER_UUID>"]}'

# Marcar parada como coletada
curl -X POST http://localhost:3000/routes/stops/<STOP_UUID>/collect
```

---

## 11. Comandos úteis

### Variáveis de ambiente (adicionar ao ~/.zshrc)

```bash
export CARDANO_NODE_SOCKET_PATH=~/cardano/preprod/node.socket
export CARDANO_NODE_MAGIC=1
```

### Scripts do projeto

| Script | Descrição | Uso |
|--------|-----------|-----|
| `scripts/setup-wallet.sh` | Gera chaves do operador | `scripts/setup-wallet.sh` |
| `scripts/setup-policy.sh` | Gera minting policy | `scripts/setup-policy.sh` |
| `scripts/split-utxos.sh` | Fragmenta UTXO do operador para operações batch | `scripts/split-utxos.sh [N]` (padrão: 10) |
| `scripts/query-balance.sh` | Consulta saldo | `scripts/query-balance.sh [ADDR\|USER_ID]` |
| `scripts/get-pubkey-hash.sh` | Extrai pubkey hash de um endereço | `scripts/get-pubkey-hash.sh <ADDR>` |

### Cardano CLI

```bash
# Verificar sync do nó
cardano-cli conway query tip --testnet-magic 1 --socket-path ~/cardano/preprod/node.socket

# Consultar UTxOs de um endereço
cardano-cli conway query utxo --address <ADDR> --testnet-magic 1 --socket-path ~/cardano/preprod/node.socket
```

### PostgreSQL (Homebrew)

```bash
# Iniciar o serviço
brew services start postgresql@14

# Parar o serviço
brew services stop postgresql@14

# Verificar status
brew services list | grep postgresql
```

---

## 12. Solução de problemas

### Nó não inicia - `GenesisHashMismatch`

O hash no `config.json` não corresponde ao arquivo genesis baixado. O nó exibe:

```
GenesisHashMismatch "hash_esperado" "hash_calculado"
```

Use o **primeiro** valor (`hash_esperado`) e atualize o campo correspondente no `config.json`.

### Nó não inicia - `NoDbMarkerAndNotEmpty`

O diretório `db/` contém dados corrompidos. Limpe e recomeçe:

```bash
rm -rf ~/cardano/preprod/db
mkdir ~/cardano/preprod/db
# Se usou Mithril, baixe o snapshot novamente
```

### Nó não inicia - `TraceOptions` / `UseTraceDispatcher`

Versões recentes do cardano-node (10.x) exigem estes campos no `config.json`:

```json
"UseTraceDispatcher": false,
"TraceOptions": {
  "ChainDB": { "severity": "Info" },
  "Net": { "severity": "Info" }
}
```

### `cardano-node` / `cardano-cli` bloqueado pelo Gatekeeper

```bash
xattr -d com.apple.quarantine ~/.local/bin/cardano-node
xattr -d com.apple.quarantine ~/.local/bin/cardano-cli
```

Se o erro persistir, acesse **Configurações do Sistema → Privacidade e Segurança** e autorize manualmente.

### `cardano-cli: command not found`

```bash
export PATH="$HOME/.local/bin:$PATH"
# Adicione ao ~/.zshrc para persistir
```

### `connect: does not exist (No such file or directory)` (socket)

O nó não está rodando ou o socket ainda não foi criado. Após iniciar o nó, aguarde ~10 segundos:

```bash
cardano-start
sleep 10
cardano-report
```

### `cardano-cli` retorna JSON inesperado

O cardano-cli 10.x mudou o output padrão para JSON em vários comandos. Os scripts do projeto já tratam esse formato. Para consultas manuais com formato texto:

```bash
cardano-cli conway query utxo --address <ADDR> --testnet-magic 1 \
  --socket-path ~/cardano/preprod/node.socket --output-text
```

### Backend não conecta no PostgreSQL

```bash
# Testar conexão
psql -U postgres -d greentoken_db -c "SELECT 1;"

# Redefinir senha se necessário
psql postgres -c "ALTER USER postgres PASSWORD 'novasenha';"

# Reiniciar o serviço PostgreSQL
brew services restart postgresql@14
```

### Transação falha - `UTxO balance insufficient`

O operador não tem tADA suficiente:

```bash
scripts/query-balance.sh
# Se < 5 ADA, envie mais tADA via faucet ou carteira Lace
```

### Mithril download - arquivo de 9 bytes ou executável bloqueado

O link direto pode ser um redirecionamento. Baixe o `.tar.gz` da página de releases:

```bash
ls -la mithril-client
# Se < 1MB, o download falhou - baixe o .tar.gz manualmente
```

Após extrair, remova o atributo de quarentena:

```bash
xattr -d com.apple.quarantine ~/cardano/mithril-client
```

### `psql: command not found` após instalar PostgreSQL via Homebrew

No Apple Silicon, adicione o PostgreSQL ao PATH:

```bash
export PATH="/opt/homebrew/opt/postgresql@14/bin:$PATH"
# Adicione ao ~/.zshrc para persistir
```

No Intel Mac:

```bash
export PATH="/usr/local/opt/postgresql@14/bin:$PATH"
```
