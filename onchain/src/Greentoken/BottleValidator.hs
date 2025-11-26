{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ImportQualifiedPost   #-}

module Greentoken.BottleValidator
  ( validator
  , wrapped
  , BottleDatum(..)
  , Action(..)
  , Stage(..)
  , mkValidator
  , serializedScript
  ) where

import Cardano.Api (PlutusScriptV2)
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS

import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx
import PlutusTx.Prelude
import Plutus.Script.Utils.Typed as Scripts

data Stage =
    Inserted
  | Compacted
  | Collected
  | AtStation
  | Shredded
PlutusTx.unstableMakeIsData ''Stage

data BottleDatum = BottleDatum
  { bdOwner    :: PlutusV2.PubKeyHash
  , bdBottleId :: BuiltinByteString
  , bdStage    :: Stage
  }
PlutusTx.unstableMakeIsData ''BottleDatum

{-# INLINABLE eqStage #-}
eqStage :: Stage -> Stage -> Bool
eqStage Inserted   Inserted   = True
eqStage Compacted  Compacted  = True
eqStage Collected  Collected  = True
eqStage AtStation  AtStation  = True
eqStage Shredded   Shredded   = True
eqStage _          _          = False

instance Eq Stage where
  {-# INLINABLE (==) #-}
  (==) = eqStage

data Action =
    AdvanceTo Stage
PlutusTx.unstableMakeIsData ''Action

{-# INLINABLE validTransition #-}
validTransition :: Stage -> Stage -> Bool
validTransition Inserted   Compacted = True
validTransition Compacted  Collected = True
validTransition Collected  AtStation = True
validTransition AtStation  Shredded  = True
validTransition old        new       = old == new

{-# INLINABLE mkValidator #-}
mkValidator :: BottleDatum -> Action -> PlutusV2.ScriptContext -> Bool
mkValidator dat act _ctx =
  let oldStage = bdStage dat
      newStage = case act of
                   AdvanceTo s -> s
  in traceIfFalse "invalid transition" (validTransition oldStage newStage)

{-# INLINABLE wrapped #-}
wrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapped = Scripts.mkUntypedValidator mkValidator

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrapped ||])

serializedScript :: PlutusScript PlutusScriptV2
serializedScript =
  PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . serialise
  $ validator
