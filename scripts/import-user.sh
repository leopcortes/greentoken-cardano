#!/usr/bin/env bash
# Script para importar um usuario a partir de uma carteira externa (ex: Lace).
# Em vez de gerar chaves via cardano-cli, recebe o endereco da carteira
# e extrai o pubkey hash automaticamente.
#
# Gera:
#   assets/users/<USER_ID>/<USER_ID>.addr
#   assets/users/<USER_ID>/<USER_ID>.pkh

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_ROOT"

source "${SCRIPT_DIR}/_db-helper.sh"

USER_ID="$1"
WALLET_ADDR="$2"
USER_NAME="${3:-$USER_ID}"
USER_EMAIL="${4:-${USER_ID}@greentoken.local}"

if [ -z "$USER_ID" ] || [ -z "$WALLET_ADDR" ]; then
  echo "Uso: $0 <USER_ID> <WALLET_ADDR> [NOME] [EMAIL]"
  echo "Exemplo: $0 user1 addr_test1qz... \"João Silva\" joao@email.com"
  exit 1
fi

# Valida que o endereco comeca com addr_test1
if [[ "$WALLET_ADDR" != addr_test1* ]]; then
  echo "ERRO: Endereco invalido. Deve comecar com addr_test1 (rede Preprod)."
  exit 1
fi

USER_DIR="assets/users/${USER_ID}"
USER_ADDR_FILE="${USER_DIR}/${USER_ID}.addr"
USER_PKH_FILE="${USER_DIR}/${USER_ID}.pkh"

mkdir -p "$USER_DIR"

# Salva o endereco
echo -n "$WALLET_ADDR" > "$USER_ADDR_FILE"

# Extrai o pubkey hash do endereco via cardano-cli address info
# O campo base16 tem: 2 chars de header + 56 chars de payment key hash (+ staking hash se base addr)
BASE16=$(cardano-cli conway address info --address "$WALLET_ADDR" | grep -oP '"base16":\s*"\K[^"]+')

if [ -z "$BASE16" ]; then
  echo "ERRO: Nao foi possivel extrair base16 do endereco."
  exit 1
fi

# Pubkey hash: 28 bytes = 56 hex chars, comecando apos o header byte (2 chars)
PKH="${BASE16:2:56}"

if [ ${#PKH} -ne 56 ]; then
  echo "ERRO: Pubkey hash extraido tem tamanho invalido (${#PKH} chars, esperado 56)."
  exit 1
fi

echo -n "$PKH" > "$USER_PKH_FILE"

echo "Usuario importado:"
echo "  USER_ID  = ${USER_ID}"
echo "  ADDR     = ${USER_ADDR_FILE}"
echo "  PKH      = ${USER_PKH_FILE}"
echo ""
echo "Endereco:"
echo "  $WALLET_ADDR"
echo ""
echo "Pubkey hash:"
echo "  $PKH"
echo ""
# Salvar no banco de dados
load_db_url && {
  EXISTING=$(db_find_user_by_addr "$WALLET_ADDR")
  if [ -n "$EXISTING" ]; then
    echo "[db] Usuario ja existe no banco (id: $EXISTING)"
  else
    DB_ID=$(db_exec "INSERT INTO users (role, name, email, wallet_address, pubkey_hash)
      VALUES ('recycler', '$USER_NAME', '$USER_EMAIL', '$WALLET_ADDR', '$PKH')
      RETURNING id;")
    echo "[db] Usuario salvo no banco (id: $DB_ID)"
  fi
}

echo ""
echo "Proximo passo: criar garrafas com"
echo "  scripts/create-bottle.sh <BOTTLE_ID> ${USER_ID}"

