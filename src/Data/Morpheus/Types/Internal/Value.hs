{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Types.Internal.Value
  ( Value(..)
  , ScalarValue(..)
  , decodeScientific
  , convertToJSONName
  , convertToHaskellName
  ) where

import qualified Data.Aeson          as A (FromJSON (..), ToJSON (..), Value (..), object, pairs, (.=))
import qualified Data.HashMap.Strict as M (toList)
import           Data.Scientific     (Scientific, floatingOrInteger)
import           Data.Semigroup      ((<>))
import           Data.Text           (Text)
import qualified Data.Vector         as V (toList)
import           GHC.Generics        (Generic)

convertToJSONName :: Text -> Text
convertToJSONName "case'"     = "case"
convertToJSONName "class'"    = "class"
convertToJSONName "data'"     = "data"
convertToJSONName "default'"  = "default"
convertToJSONName "deriving'" = "deriving"
convertToJSONName "do'"       = "do"
convertToJSONName "else'"     = "else"
convertToJSONName "foreign'"  = "foreign"
convertToJSONName "if'"       = "if"
convertToJSONName "import'"   = "import"
convertToJSONName "in'"       = "in"
convertToJSONName "infix'"    = "infix"
convertToJSONName "infixl'"   = "infixl"
convertToJSONName "infixr'"   = "infixr"
convertToJSONName "instance'" = "instance"
convertToJSONName "let'"      = "let"
convertToJSONName "module'"   = "module"
convertToJSONName "newtype'"  = "newtype"
convertToJSONName "of'"       = "of"
convertToJSONName "then'"     = "then"
convertToJSONName "type'"     = "type"
convertToJSONName "where'"    = "where"
convertToJSONName "_'"        = "_"
convertToJSONName x           = x

convertToHaskellName :: Text -> Text
convertToHaskellName "case"     = "case'"
convertToHaskellName "class"    = "class'"
convertToHaskellName "data"     = "data'"
convertToHaskellName "default"  = "default'"
convertToHaskellName "deriving" = "deriving'"
convertToHaskellName "do"       = "do'"
convertToHaskellName "else"     = "else'"
convertToHaskellName "foreign"  = "foreign'"
convertToHaskellName "if"       = "if'"
convertToHaskellName "import"   = "import'"
convertToHaskellName "in"       = "in'"
convertToHaskellName "infix"    = "infix'"
convertToHaskellName "infixl"   = "infixl'"
convertToHaskellName "infixr"   = "infixr'"
convertToHaskellName "instance" = "instance'"
convertToHaskellName "let"      = "let'"
convertToHaskellName "module"   = "module'"
convertToHaskellName "newtype"  = "newtype'"
convertToHaskellName "of"       = "of'"
convertToHaskellName "then"     = "then'"
convertToHaskellName "type"     = "type'"
convertToHaskellName "where"    = "where'"
convertToHaskellName "_"        = "_'"
convertToHaskellName x          = x

-- | Primitive Values for GQLScalar: 'Int', 'Float', 'String', 'Boolean'.
-- for performance reason type 'Text' represents GraphQl 'String' value
data ScalarValue
  = Int Int
  | Float Float
  | String Text
  | Boolean Bool
  deriving (Show, Generic)

instance A.ToJSON ScalarValue where
  toEncoding (Float x)   = A.toEncoding x
  toEncoding (Int x)     = A.toEncoding x
  toEncoding (Boolean x) = A.toEncoding x
  toEncoding (String x)  = A.toEncoding x

data Value
  = Object [(Text, Value)]
  | List [Value]
  | Enum Text
  | Scalar ScalarValue
  | Null
  deriving (Show, Generic)

instance A.ToJSON Value where
  toEncoding Null = A.toEncoding A.Null
  toEncoding (Enum x) = A.toEncoding x
  toEncoding (List x) = A.toEncoding x
  toEncoding (Scalar x) = A.toEncoding x
  toEncoding (Object []) = A.toEncoding $ A.object []
  toEncoding (Object x) = A.pairs $ foldl1 (<>) $ map encodeField x
    where
      encodeField (key, value) = convertToJSONName key A..= value

replace :: (a, A.Value) -> (a, Value)
replace (key, val) = (key, replaceValue val)

decodeScientific :: Scientific -> ScalarValue
decodeScientific v =
  case floatingOrInteger v of
    Left float -> Float float
    Right int  -> Int int

replaceValue :: A.Value -> Value
replaceValue (A.Bool v)   = Scalar $ Boolean v
replaceValue (A.Number v) = Scalar $ decodeScientific v
replaceValue (A.String v) = Scalar $ String v
replaceValue (A.Object v) = Object $ map replace (M.toList v)
replaceValue (A.Array li) = List (map replaceValue (V.toList li))
replaceValue A.Null       = Null

instance A.FromJSON Value where
  parseJSON = pure . replaceValue
