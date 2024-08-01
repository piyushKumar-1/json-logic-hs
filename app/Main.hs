{-# LANGUAGE QuasiQuotes #-}
module Main where

import JsonLogic
import Prelude
import qualified Data.Aeson.QQ.Simple as AQ

main :: IO ()
main = do
  let tests = [AQ.aesonQQ|{"filter" : { {"var": "integers"} , { "==" : [{"var": ""}, 1] }}|]
  let data_ = [AQ.aesonQQ|{ "integers": [1, 2, 3, 4, 1]}|]
  print $ jsonLogic tests data_
