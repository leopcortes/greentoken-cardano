#!/usr/bin/env bash
# Script para criar uma nova garrafa no contrato
# gera todos os datums, prepara UTXO do operador, constroi, assina e envia a transacao de mint + deposito

set -e

# Recebe BOTTLE_ID e USER_ID como parametros
BOTTLE_ID="$1"
USER_ID="$2"

# Valida parametros obrigatorios
if [ -z "$BOTTLE_ID" ] || [ -z "$USER_ID" ]; then
  echo "Uso: $0 <BOTTLE_ID> <USER_ID>"
  echo "Exemplo: $0 bottle-1237 user1"
  exit 1
fi

# Verifica variaveis do node
if [ -z "$CARDANO_NODE_MAGIC" ] || [ -z "$CARDANO_NODE_SOCKET_PATH" ]; then
  echo "Defina CARDANO_NODE_MAGIC e CARDANO_NODE_SOCKET_PATH no ambiente."
  exit 1
fi

# Caminhos dos arquivos do usuario
USER_VKEY="assets/users/${USER_ID}/${USER_ID}.vkey"
USER_ADDR_FILE="assets/users/${USER_ID}/${USER_ID}.addr"

# Verifica se os arquivos do usuario existem
if [ ! -f "$USER_VKEY" ] || [ ! -f "$USER_ADDR_FILE" ]; then
  echo "Arquivos do usuário não encontrados: $USER_VKEY ou $USER_ADDR_FILE"
  exit 1
fi

# Hash da chave publica do usuário
USER_HASH=$(cardano-cli address key-hash \
  --payment-verification-key-file "$USER_VKEY")

# Converte o ID da garrafa para hex
BOTTLE_HEX=$(echo -n "$BOTTLE_ID" | xxd -ps | tr -d '\n')

# Diretorio dos datums desta garrafa
DATUM_DIR="assets/datums/${BOTTLE_ID}"
mkdir -p "$DATUM_DIR"

DATUM_INSERTED_FILE="${DATUM_DIR}/datum-${BOTTLE_ID}-inserted.json"

# Funcao para gerar um datum para cada estagio
criar_datum() {
  local STAGE_NAME="$1"
  local STAGE_CONSTRUCTOR="$2"
  local FILE_PATH="${DATUM_DIR}/datum-${BOTTLE_ID}-${STAGE_NAME}.json"

  cat > "$FILE_PATH" <<EOF
{
  "constructor": 0,
  "fields": [
    { "bytes": "$USER_HASH" },
    { "bytes": "$BOTTLE_HEX" },
    { "constructor": $STAGE_CONSTRUCTOR, "fields": [] }
  ]
}
EOF

  echo "Criado: $FILE_PATH"
}

# Criacao dos datums para todos os estagios
criar_datum "inserted"   0
criar_datum "compacted"  1
criar_datum "collected"  2
criar_datum "atstation"  3
criar_datum "shredded"   4

# Carrega endereco do usuario e identificadores da policy/token
USER_ADDR=$(cat "$USER_ADDR_FILE")
POLICY_ID=$(cat assets/policy/policyID)
TOKEN_NAME_HEX=$(echo -n "Greentoken" | xxd -ps | tr -d '\n')
SCRIPT_ADDR=$(cat assets/wallet/bottle.addr)
OPERATOR_ADDR=$(cat assets/wallet/payment.addr)

MIN_ADA_BOTTLE=2000000
MIN_ADA_REWARD=2000000

# Busca UTXO ADA puro do operador para financiar a transacao
OPERATOR_UTXO_INFO=$(cardano-cli conway query utxo \
  --address "$OPERATOR_ADDR" \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  | awk 'NR>2 && $0 ~ /lovelace \+ TxOutDatum/ {print $1 "#" $2 " " $3}' \
  | sort -k2,2n \
  | tail -n1)

# Caso nenhum UTXO adequado seja encontrado
if [ -z "$OPERATOR_UTXO_INFO" ]; then
  echo "Nenhum UTXO somente ADA encontrado em $OPERATOR_ADDR"
  exit 1
fi

# Extrai o TX-IN e quantidade de ADA
TX_IN_OPERATOR="${OPERATOR_UTXO_INFO%% *}"
TX_IN_LOVELACE="${OPERATOR_UTXO_INFO##* }"
MIN_IN_LOVELACE=5000000

# Verifica se tem ADA suficiente
if [ "$TX_IN_LOVELACE" -lt "$MIN_IN_LOVELACE" ]; then
  echo "UTXO do operador tem apenas $TX_IN_LOVELACE lovelace, mínimo recomendado é $MIN_IN_LOVELACE"
  exit 1
fi

# Constantes de ADA mínima nas saidas
MIN_ADA_BOTTLE=2000000
MIN_ADA_USER=2000000

# Diretorio para salvar transacoes
mkdir -p assets/txs

BODY_FILE="assets/txs/mint-bottle-${BOTTLE_ID}.body"
SIGNED_FILE="assets/txs/mint-bottle-${BOTTLE_ID}.signed"

# Log de informacoes uteis para o usuario
echo "Criando garrafa $BOTTLE_ID para usuário $USER_ID"
echo "USER_HASH = $USER_HASH"
echo "BOTTLE_HEX = $BOTTLE_HEX"
echo "DATUM_INSERTED_FILE = $DATUM_INSERTED_FILE"
echo "TX_IN_OPERATOR = $TX_IN_OPERATOR"
echo "TX_IN_LOVELACE = $TX_IN_LOVELACE"

# Monta a transacao:
# 1) envia garrafa ao script com datum "Inserted"
# 2) envia 10 Greentoken ao usuario
# 3) financia e recebe troco no operador
# 4) faz o mint dos tokens
cardano-cli conway transaction build \
  --tx-in "$TX_IN_OPERATOR" \
  --tx-out "${SCRIPT_ADDR}+${MIN_ADA_BOTTLE}" \
  --tx-out-datum-hash-file "$DATUM_INSERTED_FILE" \
  --tx-out "${USER_ADDR}+${MIN_ADA_USER} + 10 ${POLICY_ID}.${TOKEN_NAME_HEX}" \
  --change-address "$OPERATOR_ADDR" \
  --mint "10 ${POLICY_ID}.${TOKEN_NAME_HEX}" \
  --minting-script-file assets/policy/policy.script \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --out-file "$BODY_FILE"

# Assina com as chaves do operador e da policy
cardano-cli conway transaction sign \
  --tx-body-file "$BODY_FILE" \
  --signing-key-file assets/wallet/payment.skey \
  --signing-key-file assets/policy/policy.skey \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --out-file "$SIGNED_FILE"

# Envia a transacao para a rede
cardano-cli conway transaction submit \
  --tx-file "$SIGNED_FILE" \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH"

echo "Garrafa $BOTTLE_ID criada com datum em $DATUM_INSERTED_FILE e 10 Greentoken enviados para $USER_ADDR."
