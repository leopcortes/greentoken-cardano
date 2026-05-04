#!/usr/bin/env bash
# Compila os contratos Aiken e gera todos os artefatos consumidos pelo backend e pelo cardano-cli:
#   assets/bottle-validator.plutus       (validador Plutus V3 da garrafa)
#   assets/greentoken-policy.plutus      (minting policy Plutus V3 do GreenToken, parametrizada)
#   assets/wallet/bottle.addr            (endereco do script da garrafa, derivado do hash)
#   assets/policy/policyID               (policy ID do GreenToken)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
AIKEN_DIR="${PROJECT_ROOT}/aiken"

if ! command -v aiken >/dev/null 2>&1; then
  echo "ERRO: comando 'aiken' nao encontrado. Instale com:"
  echo "  brew install aiken-lang/tap/aiken"
  echo "  ou siga https://aiken-lang.org/installation-instructions"
  exit 1
fi

cd "$AIKEN_DIR"

echo "==> aiken build"
aiken build

echo "==> aplicando parametro bottle_script_hash na minting policy"
BOTTLE_HASH=$(aiken blueprint hash -v bottle)
aiken blueprint apply -v greentoken -o plutus.json "581c${BOTTLE_HASH}" >/dev/null

echo "==> exportando scripts .plutus"
aiken blueprint convert -v bottle    > "${PROJECT_ROOT}/assets/bottle-validator.plutus"
aiken blueprint convert -v greentoken > "${PROJECT_ROOT}/assets/greentoken-policy.plutus"

echo "==> exportando endereco do script e policyID"
aiken blueprint address -v bottle > "${PROJECT_ROOT}/assets/wallet/bottle.addr"
aiken blueprint policy  -v greentoken | tr -d '\n' > "${PROJECT_ROOT}/assets/policy/policyID"

echo ""
echo "Artefatos gerados:"
echo "  bottle.addr   = $(cat "${PROJECT_ROOT}/assets/wallet/bottle.addr")"
echo "  policyID      = $(cat "${PROJECT_ROOT}/assets/policy/policyID")"
echo "  bottle hash   = ${BOTTLE_HASH}"
