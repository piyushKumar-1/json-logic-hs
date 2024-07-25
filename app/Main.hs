{-# LANGUAGE QuasiQuotes #-}
module Main where

import JsonLogic
import Prelude
import qualified Data.Aeson.QQ.Simple as AQ

main :: IO ()
main = do
  let tests = [AQ.aesonQQ|{ "==" : [1, 1] }|]
  let data_ = [AQ.aesonQQ|{}|]
  print $ jsonLogic tests data_
