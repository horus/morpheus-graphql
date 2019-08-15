{-# LANGUAGE OverloadedStrings #-}

module ConvertTo.PatternMatch where

import Data.Text (Text)

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
