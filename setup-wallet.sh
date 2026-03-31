#!/usr/bin/env bash
# Script para gerar as chaves do operador (payment) e derivar o endereço do script Plutus.
# Executa uma unica vez na configuracao inicial do ambiente.
#
# Gera:
#   assets/wallet/payment.vkey
#   assets/wallet/payment.skey
#   assets/wallet/payment.addr
#   assets/wallet/bottle.addr  (endereco do script Plutus)

set -e

if [ -z "$CARDANO_NODE_MAGIC" ]; then
  echo "Defina CARDANO_NODE_MAGIC no ambiente (ex: export CARDANO_NODE_MAGIC=1)."
  exit 1
fi

WALLET_DIR="assets/wallet"
VKEY="${WALLET_DIR}/payment.vkey"
SKEY="${WALLET_DIR}/payment.skey"
ADDR="${WALLET_DIR}/payment.addr"
SCRIPT_FILE="assets/bottle-validator.plutus"
SCRIPT_ADDR="${WALLET_DIR}/bottle.addr"

mkdir -p "$WALLET_DIR"

# Verifica se ja existem chaves para nao sobrescrever acidentalmente
if [ -f "$SKEY" ]; then
  echo "AVISO: $SKEY ja existe. Para gerar novas chaves, remova os arquivos existentes."
  echo "  rm $VKEY $SKEY $ADDR"
  exit 1
fi

echo "Gerando chaves do operador em ${WALLET_DIR} ..."

# 1. Gerar par de chaves
cardano-cli address key-gen \
  --verification-key-file "$VKEY" \
  --signing-key-file "$SKEY"

# 2. Derivar endereco do operador
cardano-cli address build \
  --payment-verification-key-file "$VKEY" \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --out-file "$ADDR"

# 3. Derivar endereco do script Plutus (onde ficam as garrafas)
if [ -f "$SCRIPT_FILE" ]; then
  cardano-cli address build \
    --payment-script-file "$SCRIPT_FILE" \
    --testnet-magic "$CARDANO_NODE_MAGIC" \
    --out-file "$SCRIPT_ADDR"
  echo "Endereco do script: $(cat "$SCRIPT_ADDR")"
else
  echo "AVISO: $SCRIPT_FILE nao encontrado. O endereco do script nao foi gerado."
  echo "  Execute novamente apos compilar o contrato Plutus."
fi

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
echo "Proximo passo: obter tADA no faucet:"
echo "  https://docs.cardano.org/cardano-testnets/tools/faucet"
