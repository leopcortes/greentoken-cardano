-- Implementa o validator on-chain do Greentoken
-- define estagios, datum regras de transicao e gera o script Plutus serializado

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

-- Estagios possiveis da garrafa
data Stage =
    Inserted
  | Compacted
  | Collected
  | AtStation
  | Shredded
PlutusTx.unstableMakeIsData ''Stage

-- Estrutura do datum armazenado na saida bloqueada pelo script
data BottleDatum = BottleDatum
  { bdOwner    :: PlutusV2.PubKeyHash      -- dono da garrafa
  , bdBottleId :: BuiltinByteString        -- id da garrafa
  , bdStage    :: Stage                    -- estagio atual
  }
PlutusTx.unstableMakeIsData ''BottleDatum

-- Comparacao de estagios
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

-- Acao permitida: avançar a garrafa para um estagio especafico
data Action =
    AdvanceTo Stage
PlutusTx.unstableMakeIsData ''Action

-- Regras de transicao validas entre estagios
{-# INLINABLE validTransition #-}
validTransition :: Stage -> Stage -> Bool
validTransition Inserted   Compacted = True
validTransition Compacted  Collected = True
validTransition Collected  AtStation = True
validTransition AtStation  Shredded  = True
validTransition old        new       = old == new   -- permite permanecer no mesmo estagio

-- Logica principal do validator
{-# INLINABLE mkValidator #-}
mkValidator :: BottleDatum -> Action -> PlutusV2.ScriptContext -> Bool
mkValidator dat act _ctx =
  let oldStage = bdStage dat                -- estagio atual
      newStage = case act of                -- próximo estagio
                   AdvanceTo s -> s
  in traceIfFalse "invalid transition"       -- registra erro on-chain
       (validTransition oldStage newStage)   -- verifica se e permitido

-- Adaptador para transformar o validator tipado em nao-tipado
{-# INLINABLE wrapped #-}
wrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapped = Scripts.mkUntypedValidator mkValidator

-- Validator compilado para formato Plutus v2
validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrapped ||])

-- Script serializado para ser escrito em arquivo .plutus
serializedScript :: PlutusScript PlutusScriptV2
serializedScript =
  PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . serialise
  $ validator
