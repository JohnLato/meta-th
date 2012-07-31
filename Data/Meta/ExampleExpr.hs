{-# LANGUAGE TemplateHaskell #-}

module Data.Meta.ExampleExpr where

import Data.Meta.Meta

mkPow :: Int -> Meta (Int -> Int)
mkPow 0 = Meta [| const 1 |]
mkPow n = Meta [| \x -> x * $(unMeta $ mkPow (n-1)) x |]
