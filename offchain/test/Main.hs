-- Executa todos os testes do BottleValidator

module Main where

import Test.Tasty
import qualified Greentoken.BottleValidatorSpec as GT

main :: IO ()
main = defaultMain GT.tests
