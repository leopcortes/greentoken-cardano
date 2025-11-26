{-# LANGUAGE OverloadedStrings #-}

module Greentoken.BottleValidatorSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Greentoken.BottleValidator
    ( Stage(..)
    , BottleDatum(..)
    , Action(..)
    , mkValidator
    )

import PlutusTx.Prelude (BuiltinByteString)
import qualified PlutusTx.Prelude as PlutusTx
import Plutus.V2.Ledger.Api as PlutusV2

-- Usuário fictício
dummyUser :: PubKeyHash
dummyUser = PubKeyHash "0011223344"

-- ID fictício
dummyBottleId :: BuiltinByteString
dummyBottleId = "bottle123"

-- Cria datum para o teste
mkDatum :: Stage -> BottleDatum
mkDatum st =
    BottleDatum
        { bdOwner    = dummyUser
        , bdBottleId = dummyBottleId
        , bdStage    = st
        }

-- Contexto vazio (mockado)
dummyCtx :: ScriptContext
dummyCtx = PlutusV2.ScriptContext
              { scriptContextTxInfo = undefined
              , scriptContextPurpose = undefined
              }

-- Executa o validator
run :: BottleDatum -> Stage -> Bool
run dat next =
    mkValidator dat (AdvanceTo next) dummyCtx

--------------------------------------------------------------------------------
-- TESTES
--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Bottle Validator Tests"
  [ testCase "Inserted → Compacted" $
      assertBool "" $
        run (mkDatum Inserted) Compacted

  , testCase "Compacted → Collected" $
      assertBool "" $
        run (mkDatum Compacted) Collected

  , testCase "Collected → AtStation" $
      assertBool "" $
        run (mkDatum Collected) AtStation

  , testCase "AtStation → Shredded" $
      assertBool "" $
        run (mkDatum AtStation) Shredded

    -- inválidas:

  , testCase "Inserted → AtStation (inválido)" $
      assertBool "" $
        not (run (mkDatum Inserted) AtStation)

  , testCase "Compacted → Shredded (inválido)" $
      assertBool "" $
        not (run (mkDatum Compacted) Shredded)

  , testCase "Collected → Inserted (inválido)" $
      assertBool "" $
        not (run (mkDatum Collected) Inserted)
  ]
