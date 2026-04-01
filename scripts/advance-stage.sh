#!/usr/bin/env bash
# Script para avançar uma garrafa no contrato Plutus: 
# valida a transicao, usa o datum correto, aplica o redeemer, paga recompensa e recria a saída do script

set -e

# Resolve o diretorio raiz do projeto (pai de scripts/)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_ROOT"

source "${SCRIPT_DIR}/_db-helper.sh"

# Parametros da chamada
STAGE="$1"
BOTTLE_ID="$2"
USER_ADDR="$3"
BOTTLE_TX_IN="$4"

# Validacao dos parametros obrigatorios
if [ -z "$STAGE" ] || [ -z "$BOTTLE_ID" ] || [ -z "$USER_ADDR" ] || [ -z "$BOTTLE_TX_IN" ]; then
  echo "Uso: $0 [compacted|collected|atstation|shredded] <BOTTLE_ID> <USER_ADDR> <BOTTLE_TX_IN>"
  echo "Exemplo:"
  echo "  $0 compacted bottle-1237 addr_test1... 37e24...d0c#0"
  exit 1
fi

# Define regras de transicao e recompensas
case "$STAGE" in
  compacted)
    STAGE_IN_NAME="inserted"
    STAGE_OUT_NAME="compacted"
    REDEEMER="assets/redeemers/redeemer-inserted-to-compacted.json"
    TOKEN_AMOUNT=10
    ;;
  collected)
    STAGE_IN_NAME="compacted"
    STAGE_OUT_NAME="collected"
    REDEEMER="assets/redeemers/redeemer-compacted-to-collected.json"
    TOKEN_AMOUNT=5
    ;;
  atstation)
    STAGE_IN_NAME="collected"
    STAGE_OUT_NAME="atstation"
    REDEEMER="assets/redeemers/redeemer-collected-to-atstation.json"
    TOKEN_AMOUNT=10
    ;;
  shredded)
    STAGE_IN_NAME="atstation"
    STAGE_OUT_NAME="shredded"
    REDEEMER="assets/redeemers/redeemer-atstation-to-shredded.json"
    TOKEN_AMOUNT=20
    ;;
  *)
    echo "Estágio inválido: $STAGE"
    exit 1
    ;;
esac

# Verifica ambiente do node
if [ -z "$CARDANO_NODE_MAGIC" ] || [ -z "$CARDANO_NODE_SOCKET_PATH" ]; then
  echo "Defina CARDANO_NODE_MAGIC e CARDANO_NODE_SOCKET_PATH no ambiente."
  exit 1
fi

# Caminhos dos datums para estagio atual e proximo
DATUM_DIR="assets/datums/${BOTTLE_ID}"
DATUM_IN="${DATUM_DIR}/datum-${BOTTLE_ID}-${STAGE_IN_NAME}.json"
DATUM_OUT="${DATUM_DIR}/datum-${BOTTLE_ID}-${STAGE_OUT_NAME}.json"

# Verifica existencia dos datums
if [ ! -f "$DATUM_IN" ]; then
  echo "Datum de entrada não encontrado: $DATUM_IN"
  exit 1
fi

if [ ! -f "$DATUM_OUT" ]; then
  echo "Datum de saída não encontrado: $DATUM_OUT"
  exit 1
fi

# Carrega identificadores do token e policy
POLICY_ID=$(cat assets/policy/policyID)
TOKEN_NAME_HEX=$(echo -n "Greentoken" | xxd -ps | tr -d '\n')
REWARD_ADDR="$USER_ADDR"

# Enderecos do script e do operador
SCRIPT_ADDR="$(cat assets/wallet/bottle.addr)"
OPERATOR_ADDR="$(cat assets/wallet/payment.addr)"

# Busca UTXO para colateral
OPERATOR_UTXO_INFO=$(cardano-cli conway query utxo \
  --address "$OPERATOR_ADDR" \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --output-text \
  | awk 'NR>2 && $0 ~ /lovelace \+ TxOutDatum/ {print $1 "#" $2 " " $3}' \
  | sort -k2,2n \
  | tail -n1)

# Falta colateral
if [ -z "$OPERATOR_UTXO_INFO" ]; then
  echo "Nenhum UTXO somente ADA encontrado em $OPERATOR_ADDR para colateral"
  exit 1
fi

# Extrai colateral
COLLATERAL_TX_IN="${OPERATOR_UTXO_INFO%% *}"
COLLATERAL_LOVELACE="${OPERATOR_UTXO_INFO##* }"
MIN_COLLAT_LOVELACE=4000000

# Verifica valor minimo
if [ "$COLLATERAL_LOVELACE" -lt "$MIN_COLLAT_LOVELACE" ]; then
  echo "UTXO de colateral em $OPERATOR_ADDR tem apenas $COLLATERAL_LOVELACE lovelace, mínimo recomendado é $MIN_COLLAT_LOVELACE"
  exit 1
fi

# Log de informacoes uteis para o usuario
echo "STAGE = $STAGE"
echo "BOTTLE_ID = $BOTTLE_ID"
echo "BOTTLE_TX_IN = $BOTTLE_TX_IN"
echo "DATUM_IN = $DATUM_IN"
echo "DATUM_OUT = $DATUM_OUT"
echo "REDEEMER = $REDEEMER"
echo "COLLATERAL_TX_IN = $COLLATERAL_TX_IN"
echo "COLLATERAL_LOVELACE = $COLLATERAL_LOVELACE"
echo "POLICY_ID = $POLICY_ID"
echo "TOKEN_NAME_HEX = $TOKEN_NAME_HEX"
echo "TOKEN_AMOUNT = $TOKEN_AMOUNT"
echo "REWARD_ADDR = $REWARD_ADDR"

# Constantes de ADA mínima nas saídas
MIN_ADA_BOTTLE=2000000
MIN_ADA_REWARD=2000000

# Constroi a transacao:
# 1) consome UTXO da garrafa com datum antigo
# 2) aplica redeemer do estágio
# 3) recria saida no script com dato novo
# 4) envia recompensa
# 5) inclui mint dos tokens
cardano-cli conway transaction build \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --tx-in "$BOTTLE_TX_IN" \
  --tx-in-script-file assets/bottle-validator.plutus \
  --tx-in-datum-file "$DATUM_IN" \
  --tx-in-redeemer-file "$REDEEMER" \
  --tx-in "$COLLATERAL_TX_IN" \
  --tx-in-collateral "$COLLATERAL_TX_IN" \
  --tx-out "${SCRIPT_ADDR}+${MIN_ADA_BOTTLE}" \
  --tx-out-datum-hash-file "$DATUM_OUT" \
  --tx-out "${REWARD_ADDR}+${MIN_ADA_REWARD} + ${TOKEN_AMOUNT} ${POLICY_ID}.${TOKEN_NAME_HEX}" \
  --change-address "$OPERATOR_ADDR" \
  --mint "${TOKEN_AMOUNT} ${POLICY_ID}.${TOKEN_NAME_HEX}" \
  --minting-script-file assets/policy/policy.script \
  --out-file "assets/txs/tx-advance-${BOTTLE_ID}-${STAGE}.body"

# Assina com operador e policy
cardano-cli conway transaction sign \
  --tx-body-file "assets/txs/tx-advance-${BOTTLE_ID}-${STAGE}.body" \
  --signing-key-file assets/wallet/payment.skey \
  --signing-key-file assets/policy/policy.skey \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --out-file "assets/txs/tx-advance-${BOTTLE_ID}-${STAGE}.signed"

# Extrai o tx hash
# cardano-cli 10.x retorna JSON: {"txhash":"abc..."} — extrair apenas o hash
TX_HASH_RAW=$(cardano-cli conway transaction txid \
  --tx-file "assets/txs/tx-advance-${BOTTLE_ID}-${STAGE}.signed")
TX_HASH=$(echo "$TX_HASH_RAW" | grep -oP '"txhash"\s*:\s*"\K[^"]+' || echo "$TX_HASH_RAW")

# Envia transacao para a rede
cardano-cli conway transaction submit \
  --tx-file "assets/txs/tx-advance-${BOTTLE_ID}-${STAGE}.signed" \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH"

echo ""
echo "Transição da garrafa $BOTTLE_ID para estágio '$STAGE' enviada e ${TOKEN_AMOUNT} Greentoken recompensados a $REWARD_ADDR."
echo "TX_HASH = $TX_HASH"
echo "Novo UTxO da garrafa no script: ${TX_HASH}#0"

# Salvar no banco de dados
REDEEMER_JSON=$(cat "$REDEEMER" | tr -d '\n')
DATUM_OUT_JSON=$(cat "$DATUM_OUT" | tr -d '\n')
load_db_url && {
  BOTTLE_DB_ID=$(db_find_bottle_by_text "$BOTTLE_ID")
  if [ -z "$BOTTLE_DB_ID" ]; then
    echo "[db] AVISO: Garrafa '$BOTTLE_ID' nao encontrada no banco."
  else
    # Insere a transacao pendente (o confirmation worker vai confirmar e atualizar o estagio)
    db_exec "INSERT INTO blockchain_txs (bottle_id, stage, tx_hash, status, datum_json, redeemer_json)
      VALUES ('$BOTTLE_DB_ID', '$STAGE', '$TX_HASH', 'pending', '$DATUM_OUT_JSON', '$REDEEMER_JSON');" > /dev/null
    echo "[db] Transacao pendente registrada (tx: $TX_HASH)"
  fi
}
