#!/usr/bin/env bash
# Script para consultar o saldo de um endereco.
#
# Uso:
#   ./query-balance.sh                    - mostra saldo do operador
#   ./query-balance.sh <ENDERECO>         - mostra saldo de qualquer endereco
#   ./query-balance.sh user1              - mostra saldo do usuario user1

set -e

# Resolve o diretorio raiz do projeto (pai de scripts/)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_ROOT"

TARGET="$1"

if [ -z "$CARDANO_NODE_MAGIC" ] || [ -z "$CARDANO_NODE_SOCKET_PATH" ]; then
  echo "Defina CARDANO_NODE_MAGIC e CARDANO_NODE_SOCKET_PATH no ambiente."
  exit 1
fi

# Determina o endereco alvo
if [ -z "$TARGET" ]; then
  # Sem argumento: mostra o operador
  ADDR_FILE="assets/wallet/payment.addr"
  if [ ! -f "$ADDR_FILE" ]; then
    echo "Endereco do operador nao encontrado: $ADDR_FILE"
    echo "Execute ./setup-wallet.sh primeiro."
    exit 1
  fi
  ADDRESS=$(cat "$ADDR_FILE")
  echo "Saldo do operador:"
elif [[ "$TARGET" == addr_* ]]; then
  # Argumento parece um endereco Cardano
  ADDRESS="$TARGET"
  echo "Saldo do endereco:"
else
  # Argumento eh um user ID - busca em assets/users/
  ADDR_FILE="assets/users/${TARGET}/${TARGET}.addr"
  if [ ! -f "$ADDR_FILE" ]; then
    echo "Endereco do usuario nao encontrado: $ADDR_FILE"
    exit 1
  fi
  ADDRESS=$(cat "$ADDR_FILE")
  echo "Saldo do usuario ${TARGET}:"
fi

echo "  $ADDRESS"
echo ""

cardano-cli conway query utxo \
  --address "$ADDRESS" \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH"
