#!/usr/bin/env bash
set -e

BOTTLE_ID="$1"
USER_ID="$2"

if [ -z "$BOTTLE_ID" ] || [ -z "$USER_ID" ]; then
  echo "Uso: $0 <BOTTLE_ID> <USER_ID>"
  echo "Exemplo: $0 bottle-1237 user1"
  exit 1
fi

if [ -z "$CARDANO_NODE_MAGIC" ] || [ -z "$CARDANO_NODE_SOCKET_PATH" ]; then
  echo "Defina CARDANO_NODE_MAGIC e CARDANO_NODE_SOCKET_PATH no ambiente."
  exit 1
fi

USER_VKEY="assets/users/${USER_ID}/${USER_ID}.vkey"
USER_ADDR_FILE="assets/users/${USER_ID}/${USER_ID}.addr"

if [ ! -f "$USER_VKEY" ] || [ ! -f "$USER_ADDR_FILE" ]; then
  echo "Arquivos do usuário não encontrados: $USER_VKEY ou $USER_ADDR_FILE"
  exit 1
fi

USER_HASH=$(cardano-cli address key-hash \
  --payment-verification-key-file "$USER_VKEY")

BOTTLE_HEX=$(echo -n "$BOTTLE_ID" | xxd -ps | tr -d '\n')

DATUM_DIR="assets/datums/${BOTTLE_ID}"
mkdir -p "$DATUM_DIR"

DATUM_INSERTED_FILE="${DATUM_DIR}/datum-${BOTTLE_ID}-inserted.json"

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

# Cria todos
criar_datum "inserted"   0
criar_datum "compacted"  1
criar_datum "collected"  2
criar_datum "atstation"  3
criar_datum "shredded"   4

DATUM_INSERTED_FILE="${DATUM_DIR}/datum-${BOTTLE_ID}-inserted.json"

USER_ADDR=$(cat "$USER_ADDR_FILE")
POLICY_ID=$(cat assets/policy/policyID)
TOKEN_NAME_HEX=$(echo -n "Greentoken" | xxd -ps | tr -d '\n')
SCRIPT_ADDR=$(cat assets/wallet/bottle.addr)
OPERATOR_ADDR=$(cat assets/wallet/payment.addr)

OPERATOR_UTXO_INFO=$(cardano-cli conway query utxo \
  --address "$OPERATOR_ADDR" \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  | awk 'NR>2 && $0 ~ /lovelace \+ TxOutDatum/ {print $1 "#" $2 " " $3}' \
  | sort -k2,2n \
  | tail -n1)

if [ -z "$OPERATOR_UTXO_INFO" ]; then
  echo "Nenhum UTXO somente ADA encontrado em $OPERATOR_ADDR"
  exit 1
fi

TX_IN_OPERATOR="${OPERATOR_UTXO_INFO%% *}"
TX_IN_LOVELACE="${OPERATOR_UTXO_INFO##* }"
MIN_IN_LOVELACE=5000000

if [ "$TX_IN_LOVELACE" -lt "$MIN_IN_LOVELACE" ]; then
  echo "UTXO do operador tem apenas $TX_IN_LOVELACE lovelace, mínimo recomendado é $MIN_IN_LOVELACE"
  exit 1
fi

mkdir -p assets/txs

BODY_FILE="assets/txs/mint-bottle-${BOTTLE_ID}.body"
SIGNED_FILE="assets/txs/mint-bottle-${BOTTLE_ID}.signed"

echo "Criando garrafa $BOTTLE_ID para usuário $USER_ID"
echo "USER_HASH = $USER_HASH"
echo "BOTTLE_HEX = $BOTTLE_HEX"
echo "DATUM_INSERTED_FILE = $DATUM_INSERTED_FILE"
echo "TX_IN_OPERATOR = $TX_IN_OPERATOR"
echo "TX_IN_LOVELACE = $TX_IN_LOVELACE"

cardano-cli conway transaction build \
  --tx-in "$TX_IN_OPERATOR" \
  --tx-out "${SCRIPT_ADDR}+2000000" \
  --tx-out-datum-hash-file "$DATUM_INSERTED_FILE" \
  --tx-out "${USER_ADDR}+1500000 + 10 ${POLICY_ID}.${TOKEN_NAME_HEX}" \
  --change-address "$OPERATOR_ADDR" \
  --mint "10 ${POLICY_ID}.${TOKEN_NAME_HEX}" \
  --minting-script-file assets/policy/policy.script \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --out-file "$BODY_FILE"

cardano-cli conway transaction sign \
  --tx-body-file "$BODY_FILE" \
  --signing-key-file assets/wallet/payment.skey \
  --signing-key-file assets/policy/policy.skey \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --out-file "$SIGNED_FILE"

cardano-cli conway transaction submit \
  --tx-file "$SIGNED_FILE" \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH"

echo "Garrafa $BOTTLE_ID criada com datum em $DATUM_INSERTED_FILE e 10 Greentoken enviados para $USER_ADDR."
