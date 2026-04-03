#!/usr/bin/env bash
# Extrai o pubkey hash (payment key hash) de um endereço Cardano bech32.
# Uso: ./get-pubkey-hash.sh <ENDERECO>

set -e

ADDR="$1"

if [ -z "$ADDR" ]; then
  echo "Uso: $0 <ENDERECO_CARDANO>"
  echo "Exemplo: $0 addr_test1qr35f3ukkfqhua4595870atp5uzanp2f899fj75z8gma7g5225d8dp9lzl8aqdnhdfq7ftcham4e3v3p90gqcchvam8s37vx77"
  exit 1
fi

if [[ "$ADDR" != addr* ]]; then
  echo "ERRO: Endereco invalido. Deve comecar com addr ou addr_test1."
  exit 1
fi

BASE16=$(cardano-cli conway address info --address "$ADDR" | grep -oP '"base16":\s*"\K[^"]+')

if [ -z "$BASE16" ]; then
  echo "ERRO: Nao foi possivel extrair base16 do endereco."
  exit 1
fi

# Pubkey hash: 28 bytes = 56 hex chars, apos o header byte (2 chars)
PKH="${BASE16:2:56}"

if [ ${#PKH} -ne 56 ]; then
  echo "ERRO: Pubkey hash extraido tem tamanho invalido (${#PKH} chars, esperado 56)."
  exit 1
fi

echo "$PKH"
