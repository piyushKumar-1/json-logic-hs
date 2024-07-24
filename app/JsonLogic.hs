module JsonLogic
  ( jsonLogic
  , Value(..)
  ) where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Aeson as A
import qualified Data.Tuple.Extra as DTE
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Key as AK
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)

jsonLogic :: Value -> Value -> Value
jsonLogic tests data_ =
  case tests of
    A.Object dict ->
      let operator = fst (head (AKM.toList dict))
          values = snd (head (AKM.toList dict))
      in applyOperation operator (jsonLogicValues values data_) data_
    _ -> tests

applyOperation :: Key -> [Value] -> Value -> Value
applyOperation op args _ = fromMaybe Null $ Map.lookup op operations <*> pure args

jsonLogicValues :: Value -> Value -> [Value]
jsonLogicValues (Array vals) data_ = map (`jsonLogic` data_) $ V.toList vals
jsonLogicValues val data_ = [jsonLogic val data_]

equate :: [Value] -> Value
equate = A.toJSON . go
  where 
    go [a, b] = a == b
    go _ = error "Wrong number of arguments passed"


operations :: Map.Map Key ([Value] -> Value)
operations = Map.fromList . map (DTE.first AK.fromString) $
  [ ("==", equate)
  -- , ("===", binaryOp hardEquals)
  -- , ("!=", binaryOp $ \a b -> Booll $ not (softEquals a b))
  -- , ("!==", binaryOp $ \a b -> Booll $ not (hardEquals a b))
  -- , (">", binaryOp $ \a b -> Booll $ less b a)
  -- , (">=", binaryOp $ \a b -> Booll $ less b a || softEquals a b)
  -- , ("<", binaryOp less)
  -- , ("<=", binaryOp lessOrEqual)
  -- , ("!", unaryOp $ \a -> Booll $ not (toBool a))
  -- , ("!!", Booll . all toBool)
  -- , ("%", binaryOp $ \a b -> Num $ mod (toNum a) (toNum b))
  -- , ("and", listOp $ Booll . all toBool)
  -- , ("or", listOp $ Booll . any toBool)
  -- , ("?:", ternaryOp $ \a b c -> if toBool a then b else c)
  -- , ("if", listOp if_)
  -- , ("log", unaryOp logValue)
  -- , ("in", binaryOp inOp)
  -- , ("cat", listOp $ Str . concatMap toString)
  -- , ("+", listOp $ Num . sum . map toNum)
  -- , ("*", listOp $ Num . product . map toNum)
  -- , ("-", binaryOp $ \a b -> Num $ toNum a - toNum b)
  -- , ("/", binaryOp $ \a b -> Num $ toNum a / toNum b)
  -- , ("min", listOp $ Num . minimum . map toNum)
  -- , ("max", listOp $ Num . maximum . map toNum)
  -- , ("merge", listOp merge)
  -- , ("count", listOp $ Num . fromIntegral . length . filter toBool)
  ]
