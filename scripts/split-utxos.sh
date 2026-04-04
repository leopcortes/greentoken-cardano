#!/usr/bin/env bash
# Fragmenta o UTXO unico do operador em N UTXOs menores.
# Isso eh necessario para operacoes batch (compactar, coletar, entregar)
# que precisam de 1 UTXO por garrafa na mesma transacao.
#
# Uso:
#   ./split-utxos.sh        - cria 10 UTXOs de 10 ADA (padrao)
#   ./split-utxos.sh 20     - cria 20 UTXOs de 10 ADA

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_ROOT"

NUM_UTXOS="${1:-10}"
ADA_PER_UTXO=10000000  # 10 ADA por UTXO

if [ -z "$CARDANO_NODE_MAGIC" ] || [ -z "$CARDANO_NODE_SOCKET_PATH" ]; then
  echo "Defina CARDANO_NODE_MAGIC e CARDANO_NODE_SOCKET_PATH no ambiente."
  exit 1
fi

ADDR_FILE="assets/wallet/payment.addr"
SKEY_FILE="assets/wallet/payment.skey"

if [ ! -f "$ADDR_FILE" ] || [ ! -f "$SKEY_FILE" ]; then
  echo "Wallet do operador nao encontrada. Execute ./setup-wallet.sh primeiro."
  exit 1
fi

ADDR=$(cat "$ADDR_FILE")

echo "Fragmentando em $NUM_UTXOS UTXOs de $((ADA_PER_UTXO / 1000000)) ADA cada..."
echo "Endereco: $ADDR"
echo ""

# Busca o maior UTXO disponivel
UTXO_JSON=$(cardano-cli conway query utxo \
  --address "$ADDR" \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --out-file /dev/stdout)

# Pega o UTXO com maior lovelace (ADA-only)
BEST_TXIN=""
BEST_LOVELACE=0

for key in $(echo "$UTXO_JSON" | python3 -c "import sys,json; d=json.load(sys.stdin); [print(k) for k in d]" 2>/dev/null); do
  lovelace=$(echo "$UTXO_JSON" | python3 -c "import sys,json; print(json.load(sys.stdin)['$key']['value']['lovelace'])" 2>/dev/null)
  num_keys=$(echo "$UTXO_JSON" | python3 -c "import sys,json; print(len(json.load(sys.stdin)['$key']['value']))" 2>/dev/null)

  # Apenas UTXOs ADA-only (1 chave = lovelace)
  if [ "$num_keys" = "1" ] && [ "$lovelace" -gt "$BEST_LOVELACE" ]; then
    BEST_TXIN="$key"
    BEST_LOVELACE="$lovelace"
  fi
done

if [ -z "$BEST_TXIN" ]; then
  echo "ERRO: Nenhum UTXO ADA-only encontrado."
  exit 1
fi

TOTAL_NEEDED=$(( NUM_UTXOS * ADA_PER_UTXO + 5000000 ))  # +5 ADA margem para taxa
if [ "$BEST_LOVELACE" -lt "$TOTAL_NEEDED" ]; then
  echo "ERRO: UTXO insuficiente. Tem $((BEST_LOVELACE / 1000000)) ADA, precisa de ~$((TOTAL_NEEDED / 1000000)) ADA."
  exit 1
fi

echo "Usando UTXO: $BEST_TXIN ($((BEST_LOVELACE / 1000000)) ADA)"
echo ""

# Monta os --tx-out
TX_OUTS=""
for i in $(seq 1 "$NUM_UTXOS"); do
  TX_OUTS="$TX_OUTS --tx-out ${ADDR}+${ADA_PER_UTXO}"
done

# Build
BODY_FILE=$(mktemp /tmp/greentoken-split-XXXXXX.body)
SIGNED_FILE=$(mktemp /tmp/greentoken-split-XXXXXX.signed)

cardano-cli conway transaction build \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --tx-in "$BEST_TXIN" \
  $TX_OUTS \
  --change-address "$ADDR" \
  --out-file "$BODY_FILE"

# Sign
cardano-cli conway transaction sign \
  --tx-body-file "$BODY_FILE" \
  --signing-key-file "$SKEY_FILE" \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --out-file "$SIGNED_FILE"

# Submit
cardano-cli conway transaction submit \
  --tx-file "$SIGNED_FILE" \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH"

# Cleanup
rm -f "$BODY_FILE" "$SIGNED_FILE"

echo ""
echo "Transacao submetida com sucesso!"
echo "Criados $NUM_UTXOS UTXOs de $((ADA_PER_UTXO / 1000000)) ADA + 1 UTXO de troco."
echo "Aguarde ~20 segundos e verifique com: scripts/query-balance.sh"
