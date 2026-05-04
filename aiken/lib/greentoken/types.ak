pub type Stage {
  Inserted
  Compacted
  Collected
  AtStation
  Shredded
}

pub type BottleDatum {
  owner: ByteArray,
  bottle_id: ByteArray,
  stage: Stage,
}

pub type BottleAction {
  AdvanceTo { target: Stage }
}

pub type MintAction {
  MintFor { stage: Stage }
}

pub fn stage_reward(s: Stage) -> Int {
  when s is {
    Inserted -> 10
    Compacted -> 3
    Collected -> 7
    AtStation -> 10
    Shredded -> 20
  }
}

pub fn valid_transition(from: Stage, to: Stage) -> Bool {
  when from is {
    Inserted ->
      when to is {
        Compacted -> True
        _ -> False
      }
    Compacted ->
      when to is {
        Collected -> True
        _ -> False
      }
    Collected ->
      when to is {
        AtStation -> True
        _ -> False
      }
    AtStation ->
      when to is {
        Shredded -> True
        _ -> False
      }
    Shredded -> False
  }
}
