# Bottle Recycling on Cardano (Plutus V2)

Projeto baseado no `plutus-v2-starter-kit`, estendido para modelar o ciclo de vida de garrafas em um contrato Plutus, com recompensas em um token nativo (`Greentoken`) para cada etapa concluída.

## Estado Atual do Projeto

- Contrato on-chain: suporta múltiplas garrafas em paralelo.

- Implementação off-chain (scripts em bash): trabalha “uma garrafa por vez”, escolhida manualmente via BOTTLE_TX_IN.

## Próximos Passos

Fluxo alvo:

`Usuário → Container → IA → Backend → Blockchain → Backend → Usuário`

Ideias:

- Modelagem de dados no banco:

  - Tabela de usuários (id, wallet, pubkey_hash etc)

  - Tabela de garrafas (id, bottle_id_text, bottle_id_hex, relação com usuário, stage_atual, utxo_hash, utxo_index, datas etc)

- Serviço off-chain responsável por:

  - Hardware envia eventos ao backend (garrafa depositada no container A, garrafa processada na estação B).

  - Detectar novas garrafas / transições

  - Montar datums e redeemers de cada garrafa

  - Construir, assinar e enviar transações (atualmente com cardano-cli)

  - Sincronizar DB com estado on-chain

  - Recompensa em Greentoken para carteira do usuário a cada estágio concluído com sucesso.

---

# Execução

## Configuração

```bash
cabal update
cabal build
cabal test
cabal run write-bottle-validator
```

## 1. Criar nova garrafa

```bash
./create-bottle.sh <bottle-id> <user-id>
```

O script cuida de:

- gerar os datums da garrafa,
- criar o UTxO no endereço do script (bottle.addr),
- enviar 10 Greentoken + ADA para o usuário.

## 2. Verificar garrafas existentes e seus estados

Para verificar as garrfas já criadas, seus hashes Tx e hashes de estado: 

```bash
cardano-cli conway query utxo \
  --address "$(cat assets/wallet/bottle.addr)" \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH"
```

Para verificar os hashes de cada state (exemplo):

```bash
cardano-cli conway transaction hash-script-data \
  --script-data-file assets/datums/bottle-<id>/datum-bottle-<id>-<state>.json
```

## 3. Realizar operações sobre as garrafas

Usando o script `advance-stage.sh`:

1. Selecionar `<BOTTLE_ID>` da garrafa que deseja realizar operações.

2. Selecionar `<STAGE>` que deseja avançar para: compacted, collected, atstation, shredded.

3. Definir variável auxiliar `$USER_ADDR` do hash de endereço do usuário que inseriu a garrafa:

```bash
USER_ADDR="$(cat assets/users/user1/user1.addr)"
```

4. Definir variável auxiliar `$BOTTLE_TX_IN` do combo `TxHash#TxIx` da garrafa que deseja realizar operações:

```bash
cardano-cli conway query utxo \
  --address "$(cat assets/wallet/bottle.addr)" \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH"

BOTTLE_TX_IN="TxHash#TxIx"
```

5. Executar

```bash
./advance-stage.sh <STAGE> <BOTTLE_ID> "$USER_ADDR" "$BOTTLE_TX_IN"
```

## Verificar estados e saldos

Ver estados das garrafas:

```bash
cardano-cli conway query utxo \
  --address $(cat assets/wallet/bottle.addr) \
  --testnet-magic $CARDANO_NODE_MAGIC \
  --socket-path $CARDANO_NODE_SOCKET_PATH
```

Ver saldo do usuário:

```bash
cardano-cli conway query utxo \
  --address "$(cat assets/users/user1/user1.addr)" \
  --testnet-magic "$CARDANO_NODE_MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH"
```

---

# Minting Greentoken

Regras:

1. Nome do token: Greentoken.

2. Quantidade: inserted = 10; compacted = 5; collected = 5; atstation = 10; shredded = 10.

3. Sem time-lock.

4. Política: assinatura única (chave de política).

## Como foi criado

Criação da chave de política:

```bash
cardano-cli address key-gen \
  --verification-key-file assets/policy/policy.vkey \
  --signing-key-file assets/policy/policy.skey
```

Script de política:

```bash
echo "{" > assets/policy/policy.script
echo "  \"keyHash\": \"$(cardano-cli address key-hash --payment-verification-key-file assets/policy/policy.vkey)\"," >> assets/policy/policy.script
echo "  \"type\": \"sig\"" >> assets/policy/policy.script
echo "}" >> assets/policy/policy.script
```

Policy ID:

```bash
cardano-cli conway transaction policyid \
  --script-file assets/policy/policy.script \
  > assets/policy/policyID
```

Variáveis auxiliares:

```bash
testnet="--testnet-magic $CARDANO_NODE_MAGIC"
address=$(cat assets/wallet/payment.addr)
policyid=$(cat assets/policy/policyID)
tokenname=$(echo -n "Greentoken" | xxd -ps | tr -d '\n')
tokenamount=10
```

### Exemplo de mint (já é feito no script )

```bash
cardano-cli conway query utxo \
  --address "$address" \
  $testnet \
  --socket-path "$CARDANO_NODE_SOCKET_PATH"

TXHASH="6a4095d3e53c73db0dd0889d80ef053df39a60bbe4945238f2c0810a55fa7602"
TXIX="1"
TX_IN="${TXHASH}#${TXIX}"

cardano-cli conway transaction build \
  $testnet \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --tx-in "$TX_IN" \
  --tx-out "$address+2000000 + ${tokenamount} ${policyid}.${tokenname}" \
  --change-address "$address" \
  --mint "${tokenamount} ${policyid}.${tokenname}" \
  --minting-script-file assets/policy/policy.script \
  --out-file assets/txs/mint-greentoken.body

cardano-cli conway transaction sign \
  --tx-body-file assets/txs/mint-greentoken.body \
  --signing-key-file assets/wallet/payment.skey \
  --signing-key-file assets/policy/policy.skey \
  $testnet \
  --out-file assets/txs/mint-greentoken.signed

cardano-cli conway transaction submit \
  --tx-file assets/txs/mint-greentoken.signed \
  $testnet \
  --socket-path "$CARDANO_NODE_SOCKET_PATH"
```

---

# Ambiente de Desenvolvimento

O projeto atualmente está sendo rodado num workspace do emulador Demeter, passos para configurar:

1. Acessar `https://demeter.run/products/starter-kits`, realizar logine e acessar a aba "`Console`".

2. Na seção "`Ports`" criar uma porta "`Cardano Node`" do tipo "`cardano-preprod`".

3. Na seção "`Workspaces`" criar um workspace com o repositório atual na branch `main`.

4. Selecionar os seguintes toolchains: `Plutus Tx` e `NodeJS`.

5. Selecionar os seguintes extras: `Caradno Binaries`, `Cardano CLI (Edge)`, `Cabal Cache`, `Nix` e `Jupyter Notebook`.

6. Selecionar o workspace size como "`Large`".

7. Selecionar a network como "`Preprod`".

8. Esperar a configuração do workspace e então abrir o VsCode.
