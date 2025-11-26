{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BSL
import           Cardano.Api (writeFileTextEnvelope, FileError, PlutusScriptV2)
import           Cardano.Api.Shelley (PlutusScript(..))

import Greentoken.BottleValidator (serializedScript)

main :: IO ()
main = do
  let script = serializedScript
  result <- writeFileTextEnvelope "bottle-validator.plutus" Nothing script
  case result of
    Left err  -> print (err :: FileError ())
    Right ()  -> putStrLn "Escrito bottle-validator.plutus"
