module Language.NativeJS.Parser.Modules.Property where
import qualified Language.NativeJS.Parser.Lexer as L
import qualified Text.Parsec as P
import qualified Language.NativeJS.CST.Property as PP
import qualified Data.Functor as F

parseProperty :: Monad m => L.Parser m PP.Property
parseProperty = P.choice [
    parseIdentifier,
    parseString,
    parseNumber
  ]

parseIdentifier :: Monad m => L.Parser m PP.Property
parseIdentifier = L.lexeme $ L.identifier F.<&> PP.Identifier

parseString :: Monad m => L.Parser m PP.Property
parseString = L.lexeme $ fromString <$> L.stringLiteral F.<&> PP.String

parseNumber :: Monad m => L.Parser m PP.Property
parseNumber = L.lexeme $ L.integer F.<&> PP.Number