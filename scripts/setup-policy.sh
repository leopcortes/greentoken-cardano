#!/usr/bin/env bash
# A minting policy do GreenToken e um script Plutus parametrizado, gerado pelo
# pipeline Aiken. Esse script apenas invoca scripts/build-contracts.sh para
# manter a interface historica de setup.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
exec "${SCRIPT_DIR}/build-contracts.sh"
