#!/usr/bin/env bash
# Script para consultar UTxOs no endereco do script Plutus.
# Util para encontrar o BOTTLE_TX_IN necessario no advance-stage.sh.
#
# Uso:
#   ./query-bottle.sh                 — lista todos os UTxOs no script
#   ./query-bottle.sh <TX_HASH>       — filtra por tx hash especifico

set -e

TX_HASH_FILTER="$1"

if [ -z "$CARDANO_NODE_MAGIC" ] || [ -z "$CARDANO_NODE_SOCKET_PATH" ]; then
  echo "Defina CARDANO_NODE_MAGIC e CARDANO_NODE_SOCKET_PATH no ambiente."
  exit 1
fi

SCRIPT_ADDR_FILE="assets/wallet/bottle.addr"

if [ ! -f "$SCRIPT_ADDR_FILE" ]; then
  echo "Endereco do script nao encontrado: $SCRIPT_ADDR_FILE"
  echo "Execute ./setup-wallet.sh primeiro."
  exit 1
fi

SCRIPT_ADDR=$(cat "$SCRIPT_ADDR_FILE")

echo "Consultando UTxOs no endereco do script:"
echo "  $SCRIPT_ADDR"
echo ""

if [ -n "$TX_HASH_FILTER" ]; then
  echo "Filtro: $TX_HASH_FILTER"
  echo ""
  cardano-cli conway query utxo \
    --address "$SCRIPT_ADDR" \
    --testnet-magic "$CARDANO_NODE_MAGIC" \
    --socket-path "$CARDANO_NODE_SOCKET_PATH" \
    --tx-in "${TX_HASH_FILTER}#0"
else
  cardano-cli conway query utxo \
    --address "$SCRIPT_ADDR" \
    --testnet-magic "$CARDANO_NODE_MAGIC" \
    --socket-path "$CARDANO_NODE_SOCKET_PATH"
fi
