use aiken/collection/list
use cardano/transaction.{OutputReference, Transaction, InlineDatum, find_input}
use greentoken/types.{
  AdvanceTo, BottleAction, BottleDatum, Inserted, Compacted, Collected,
  AtStation, Shredded, valid_transition,
}

validator bottle {
  spend(
    datum_opt: Option<BottleDatum>,
    redeemer: BottleAction,
    own_ref: OutputReference,
    self: Transaction,
  ) {
    expect Some(datum) = datum_opt
    let AdvanceTo { target } = redeemer

    expect Some(own_input) = find_input(self.inputs, own_ref)
    let own_address = own_input.output.address

    let continuing_outputs =
      list.filter(self.outputs, fn(out) { out.address == own_address })

    expect [continuing] = continuing_outputs
    expect InlineDatum(out_data) = continuing.datum
    expect new_datum: BottleDatum = out_data

    and {
      valid_transition(datum.stage, target),
      new_datum.owner == datum.owner,
      new_datum.bottle_id == datum.bottle_id,
      new_datum.stage == target,
    }
  }

  else(_) {
    fail
  }
}

test transition_inserted_to_compacted_ok() {
  valid_transition(Inserted, Compacted)
}

test transition_compacted_to_collected_ok() {
  valid_transition(Compacted, Collected)
}

test transition_collected_to_atstation_ok() {
  valid_transition(Collected, AtStation)
}

test transition_atstation_to_shredded_ok() {
  valid_transition(AtStation, Shredded)
}

test transition_inserted_to_atstation_fails() {
  !valid_transition(Inserted, AtStation)
}

test transition_compacted_to_shredded_fails() {
  !valid_transition(Compacted, Shredded)
}

test transition_collected_to_inserted_fails() {
  !valid_transition(Collected, Inserted)
}

test transition_shredded_to_anything_fails() {
  !valid_transition(Shredded, Inserted) && !valid_transition(
    Shredded,
    Compacted,
  ) && !valid_transition(Shredded, Shredded)
}

test transition_self_loops_fail() {
  !valid_transition(Inserted, Inserted) && !valid_transition(
    Compacted,
    Compacted,
  )
}
