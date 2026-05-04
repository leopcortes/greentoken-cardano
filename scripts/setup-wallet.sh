#!/usr/bin/env bash
# Gera o par de chaves do operador (paga as taxas de todas as transacoes do sistema).
# Executa uma unica vez na configuracao inicial do ambiente.
#
# Gera:
#   assets/wallet/payment.vkey
#   assets/wallet/payment.skey
#   assets/wallet/payment.addr
#
# O endereco do script Plutus (assets/wallet/bottle.addr) e gerado por scripts/build-contracts.sh.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_ROOT"

if [ -z "${CARDANO_NODE_MAGIC:-}" ]; then
  echo "Defina CARDANO_NODE_MAGIC no ambiente (ex: export CARDANO_NODE_MAGIC=1)."
  exit 1
fi

WALLET_DIR="assets/wallet"
VKEY="${WALLET_DIR}/payment.vkey"
SKEY="${WALLET_DIR}/payment.skey"
ADDR="${WALLET_DIR}/payment.addr"

mkdir -p "$WALLET_DIR"

if [ -f "$SKEY" ]; then
  echo "AVISO: $SKEY ja existe. Para gerar novas chaves, remova os arquivos existentes."
  echo "  rm $VKEY $SKEY $ADDR"
  exit 1
fi

echo "Gerando chaves do operador em ${WALLET_DIR} ..."

cardano-cli address key-gen \
  --verification-key-file "$VKEY" \
  --signing-key-file "$SKEY"

cardano-cli address build \
  --payment-verification-key-file "$VKEY" \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --out-file "$ADDR"

echo ""
echo "Operador configurado:"
echo "  VKEY = $VKEY"
echo "  SKEY = $SKEY"
echo "  ADDR = $ADDR"
echo ""
echo "Endereco do operador (envie tADA para este endereco):"
cat "$ADDR"
echo ""
echo ""
echo "Proximos passos:"
echo "  1. Obter tADA no faucet: https://docs.cardano.org/cardano-testnets/tools/faucet"
echo "  2. Compilar e exportar os contratos: scripts/build-contracts.sh"
