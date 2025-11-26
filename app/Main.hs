{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import System.Environment (getArgs)
import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Greentoken.BottleValidator qualified as GT

writePlutusScript :: FilePath -> PlutusScript PlutusScriptV2 -> IO ()
writePlutusScript file script = do
  result <- writeFileTextEnvelope file Nothing script
  case result of
    Left err  -> putStrLn (displayError err)
    Right ()  -> pure ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [outFile] -> writePlutusScript outFile GT.serializedScript
    _         -> putStrLn "Usage: plutus-starter-kit <output-file>"
