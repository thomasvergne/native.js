{-# OPTIONS_GHC -Wno-orphans #-}
module Language.NativeJS.CST.Modules.Literal where
import Prelude hiding (Undefined)
import qualified Text.Show as T

data Literal
  = Char Char
  | String Text
  | Int Integer
  | Float Double
  | Bool Bool
  | Null
  | Undefined
  deriving (Eq, Ord)

instance T.Show Literal where
  show (Char c) = show c
  show (String s) = show s
  show (Int i) = show i
  show (Float f) = show f
  show (Bool b) = show b
  show Null = "null"
  show Undefined = "undefined"