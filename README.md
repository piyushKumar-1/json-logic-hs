# json-logic-hs

This parser accepts [JsonLogic](http://jsonlogic.com) rules and executes them in Haskell

This is a Haskell porting of the excellent GitHub project by [jwadhams](https://github.com/jwadhams) for JavaScript: [json-logic-js](https://github.com/jwadhams/json-logic-js).

All credit goes to him, this is simply an implementation of the same logic in Haskell (small differences below).

The JsonLogic format is designed to allow you to share rules (logic) between front-end and back-end code (regardless of language difference), even to store logic along with a record in a database.  JsonLogic is documented extensively at [JsonLogic.com](http://jsonlogic.com), including examples of every [supported operation](http://jsonlogic.com/operations.html) and a place to [try out rules in your browser](http://jsonlogic.com/play.html).

The same format can also be executed in PHP by the library [json-logic-php](https://github.com/jwadhams/json-logic-php/)

## Examples

### Simple
```haskell
import JsonLogic
import Data.Aeson as A
import qualified Data.Aeson.QQ.Simple as AQ

test :: IO ()
test = do
  let tests = [AQ.aesonQQ|{ "==": [1,1]}|]
  let data_ = A.Null
  print $ jsonLogic tests data_
