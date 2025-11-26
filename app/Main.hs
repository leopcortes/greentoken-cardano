-- Gera um arquivo contendo o script Plutus do BottleValidator a partir do modulo Greentoken.BottleValidator

{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import System.Environment (getArgs)
import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Greentoken.BottleValidator qualified as GT

-- Função que grava o script Plutus em arquivo
writePlutusScript :: FilePath -> PlutusScript PlutusScriptV2 -> IO ()
writePlutusScript file script = do
  result <- writeFileTextEnvelope file Nothing script
  case result of
    Left err  -> putStrLn (displayError err)   -- Exibe erro de gravacao
    Right ()  -> pure ()                       -- Sucesso

main :: IO ()
main = do
  args <- getArgs
  case args of
    [outFile] -> writePlutusScript outFile GT.serializedScript   -- Grava o script no arquivo informado
    _         -> putStrLn "Usage: bottle-validator-cli <output-file>"   -- Uso incorreto
