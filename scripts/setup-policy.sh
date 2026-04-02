#!/usr/bin/env bash
# Script para gerar as chaves da minting policy e computar o policyID.
# Executa uma unica vez na configuracao inicial do ambiente.
#
# Gera:
#   assets/policy/policy.vkey
#   assets/policy/policy.skey
#   assets/policy/policy.script  (native script de assinatura simples)
#   assets/policy/policyID

set -e

# Resolve o diretorio raiz do projeto (pai de scripts/)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_ROOT"

POLICY_DIR="assets/policy"
VKEY="${POLICY_DIR}/policy.vkey"
SKEY="${POLICY_DIR}/policy.skey"
SCRIPT="${POLICY_DIR}/policy.script"
POLICY_ID_FILE="${POLICY_DIR}/policyID"

mkdir -p "$POLICY_DIR"

# Verifica se ja existem chaves para nao sobrescrever
if [ -f "$SKEY" ]; then
  echo "AVISO: $SKEY ja existe. Para gerar novas chaves, remova os arquivos existentes."
  echo "  rm $VKEY $SKEY $SCRIPT $POLICY_ID_FILE"
  exit 1
fi

echo "Gerando chaves da minting policy em ${POLICY_DIR} ..."

# 1. Gerar par de chaves
cardano-cli address key-gen \
  --verification-key-file "$VKEY" \
  --signing-key-file "$SKEY"

# 2. Extrair o key hash
KEY_HASH=$(cardano-cli address key-hash \
  --payment-verification-key-file "$VKEY")

# 3. Gerar o native script (assinatura simples - quem possui a skey pode mintar)
cat > "$SCRIPT" <<EOF
{
  "keyHash": "$KEY_HASH",
  "type": "sig"
}
EOF

# 4. Computar o policyID
POLICY_ID=$(cardano-cli conway transaction policyid \
  --script-file "$SCRIPT")
echo -n "$POLICY_ID" > "$POLICY_ID_FILE"

echo ""
echo "Minting policy configurada:"
echo "  VKEY      = $VKEY"
echo "  SKEY      = $SKEY"
echo "  SCRIPT    = $SCRIPT"
echo "  POLICY_ID = $POLICY_ID"
echo ""
echo "IMPORTANTE: Se voce regenerou as chaves, o policyID mudou."
echo "  Tokens mintados com a policy anterior NAO sao compativeis."
