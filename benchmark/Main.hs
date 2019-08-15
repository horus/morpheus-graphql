{-# LANGUAGE OverloadedStrings #-}

import           Criterion.Main
import qualified ConvertTo.PatternMatch as PM
import qualified ConvertTo.IsReserved as CheckFirst

main :: IO ()
main = defaultMain
  [ bgroup
      "to-hs"
      [ bench "def" (whnf PM.convertToHaskellName "helloworld")
      , bench "def-kw" (whnf PM.convertToHaskellName "infix")
      , bench "checkfirst" (whnf CheckFirst.convertToHaskellName "helloworld")
      , bench "checkfirst-kw" (whnf CheckFirst.convertToHaskellName "infix")]
  , bgroup
      "to-json"
      [ bench "def" (whnf PM.convertToJSONName "helloworld")
      , bench "def-kw" (whnf PM.convertToJSONName "where")
      , bench "checkfirst" (whnf CheckFirst.convertToJSONName "helloworld")
      , bench "checkfirst-kw" (whnf CheckFirst.convertToJSONName "where")]]
