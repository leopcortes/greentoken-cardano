#!/usr/bin/env bash
# Script para criar um novo usuario no sistema:
# gera par de chaves e endereço em assets/users/<USER_ID>/

set -e

USER_ID="$1"

if [ -z "$USER_ID" ]; then
  echo "Uso: $0 <USER_ID>"
  echo "Exemplo: $0 user3"
  exit 1
fi

if [ -z "$CARDANO_NODE_MAGIC" ]; then
  echo "Defina CARDANO_NODE_MAGIC no ambiente (ex: export CARDANO_NODE_MAGIC=2)."
  exit 1
fi

USER_DIR="assets/users/${USER_ID}"
USER_VKEY="${USER_DIR}/${USER_ID}.vkey"
USER_SKEY="${USER_DIR}/${USER_ID}.skey"
USER_ADDR="${USER_DIR}/${USER_ID}.addr"

mkdir -p "$USER_DIR"

echo "Gerando chaves para usuário ${USER_ID} em ${USER_DIR} ..."

cardano-cli address key-gen \
  --verification-key-file "$USER_VKEY" \
  --signing-key-file "$USER_SKEY"

cardano-cli address build \
  --payment-verification-key-file "$USER_VKEY" \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --out-file "$USER_ADDR"

echo "Usuário criado:"
echo "  USER_ID      = ${USER_ID}"
echo "  VKEY         = ${USER_VKEY}"
echo "  SKEY         = ${USER_SKEY}"
echo "  ADDR         = ${USER_ADDR}"
echo
echo "Endereço do usuário:"
cat "$USER_ADDR"
echo
