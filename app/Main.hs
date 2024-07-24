{-# LANGUAGE QuasiQuotes #-}
module Main where

import JsonLogic
import Data.Aeson.KeyMap as AKM
import Data.Aeson.Key as AK
import qualified Data.Tuple.Extra as DTU
import Data.Aeson as A
import qualified Data.Aeson.QQ.Simple as AQ

main :: IO ()
main = do
  let tests = [AQ.aesonQQ|{ "==": [1,1]}|]
  let data_ = A.Null
  print $ jsonLogic tests data_
