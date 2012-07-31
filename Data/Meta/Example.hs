{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wall #-}

module Data.Meta.Example where

import Data.Meta.Meta
import Data.Meta.ExampleExpr


main :: IO ()
main = do
  n1 <- readLn
  let expr1 = mkPow n1 :: Meta (Int -> Int)
  n2 <- readLn
  let expr2 = mkPow n2 :: Meta (Int -> Int)
      expr3 = compose expr1 expr2
  result <- metaCompile expr3
  case result of
    Left err -> putStrLn "failed" >> putStrLn err
    Right fn -> do
      j <- readLn
      print $ fn j
      k <- readLn
      print $ fn k
