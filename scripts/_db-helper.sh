#!/usr/bin/env bash
# Helper compartilhado para integrar os scripts bash com o PostgreSQL.
# Sourced pelos demais scripts - nao deve ser executado diretamente.

# Carrega DATABASE_URL do backend/.env
# Retorna 1 se nao encontrar (scripts continuam sem gravar no banco)
load_db_url() {
  local env_file="${PROJECT_ROOT}/backend/.env"
  if [ ! -f "$env_file" ]; then
    echo "[db] AVISO: backend/.env nao encontrado. Dados NAO serao salvos no banco."
    return 1
  fi
  DATABASE_URL=$(grep -E '^DATABASE_URL=' "$env_file" | cut -d'=' -f2-)
  if [ -z "$DATABASE_URL" ]; then
    echo "[db] AVISO: DATABASE_URL nao definido em backend/.env."
    return 1
  fi
  export DATABASE_URL
  DB_AVAILABLE=true
  return 0
}

# Executa SQL e retorna o resultado (sem headers, sem command tags)
db_exec() {
  if [ "$DB_AVAILABLE" != "true" ]; then return 1; fi
  local result
  result=$(psql "$DATABASE_URL" -t -A -q -c "$1" 2>&1) || true
  # Remove linhas vazias do resultado
  echo "$result" | grep -v '^$' || true
}

# Busca UUID de um usuario pelo wallet_address
db_find_user_by_addr() {
  local addr="$1"
  db_exec "SELECT id FROM users WHERE wallet_address = '$addr' LIMIT 1;"
}

# Busca UUID de um usuario pelo nome
db_find_user_by_name() {
  local name="$1"
  db_exec "SELECT id FROM users WHERE name = '$name' LIMIT 1;"
}

# Busca UUID de uma garrafa pelo bottle_id_text
db_find_bottle_by_text() {
  local bottle_text="$1"
  db_exec "SELECT id FROM bottles WHERE bottle_id_text = '$bottle_text' LIMIT 1;"
}

DB_AVAILABLE=false
