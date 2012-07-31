{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wall #-}
module Data.Meta.Meta (
-- * Meta type
  Meta (..)

-- ** helpers/instances
, compose

-- * Functions
, metaCompile
) where

import Language.Haskell.TH

import Data.Typeable as Typ
import Control.Exception (bracket)

import System.Plugins  -- from plugins
import System.IO
import System.Directory


newtype Meta a = Meta { unMeta :: ExpQ }

compose :: Meta (a -> b) -> Meta (b -> c) -> Meta (a -> c)
compose (Meta l) (Meta r) = Meta [| $r . $l |]


-- | Super-dodgy for the moment, the Meta type should register the
-- imports it needs.
metaCompile :: forall a. Typeable a => Meta a -> IO (Either String a)
metaCompile (Meta expr) = do
  expr' <- runQ expr
  let interpStr = pprint expr'
      typeTypeRep = Typ.typeOf (undefined :: a)

  let opener = do
        (tfile, h) <- openTempFile "." "fooTmpFile.hs"
        hPutStr h (unlines
              [ "module TempMod where"
              , "import Prelude"
              , "import Language.Haskell.TH"
              , "import GHC.Num"
              , "import GHC.Base"
              , ""
              , "myFunc :: " ++ show typeTypeRep
              , "myFunc = " ++ interpStr] )
        hFlush h
        hClose h
        return tfile
  bracket opener removeFile $ \tfile -> do
  
      res <- make tfile ["-O2", "-ddump-simpl"]
      let ofile = case res of
                    MakeSuccess _ fp -> fp
                    MakeFailure errs -> error $ show errs
      print $ "loading from: " ++ show ofile
      r2 <- load (ofile) [] [] "myFunc"
      print "loaded"

      case r2 of
        LoadFailure er  -> return (Left (show er))
        LoadSuccess _ (fn :: a) -> return $ Right fn
