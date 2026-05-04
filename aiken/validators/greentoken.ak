use aiken/collection/list
use cardano/address.{Credential, Script}
use cardano/assets.{PolicyId, quantity_of}
use cardano/transaction.{InlineDatum, Output, Transaction}
use greentoken/types.{BottleDatum, MintAction, MintFor, stage_reward}

validator greentoken(bottle_script_hash: ByteArray) {
  mint(redeemer: MintAction, policy_id: PolicyId, self: Transaction) {
    let MintFor { stage } = redeemer
    let token_name = "Greentoken"
    let expected_amount = stage_reward(stage)

    let minted_amount = quantity_of(self.mint, policy_id, token_name)

    let bottle_outputs =
      list.filter(
        self.outputs,
        fn(out: Output) {
          when out.address.payment_credential is {
            Script(hash) -> hash == bottle_script_hash
            _ -> False
          }
        },
      )

    let matching =
      list.filter(
        bottle_outputs,
        fn(out: Output) {
          when out.datum is {
            InlineDatum(d) -> {
              expect bd: BottleDatum = d
              bd.stage == stage
            }
            _ -> False
          }
        },
      )

    and {
      minted_amount == expected_amount,
      list.length(matching) == 1,
    }
  }

  else(_) {
    fail
  }
}
