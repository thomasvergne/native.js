module Language.NativeJS.CST.Property where

-- Representing a property in an object literal.
data Property
  = Identifier Text
  -- ^ A property with an identifier as its name.
  | String Text
  -- ^ A property with a string as its name.
  | Number Integer
  -- ^ A property with a number as its name.
  deriving (Eq, Show, Ord)
