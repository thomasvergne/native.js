module Language.NativeJS.Parser.Modules.Literal where
import qualified Language.NativeJS.CST.Modules.Literal as C
import qualified Language.NativeJS.Parser.Lexer as L
import qualified Text.Parsec as P
import qualified Data.Functor as F

parseLiteral :: Monad m => L.Parser m C.Literal
parseLiteral = P.choice [
    C.Int <$> L.integer,
    C.Float <$> L.float,
    C.Bool <$> (L.reserved "true" F.$> True P.<|> L.reserved "false" F.$> False),
    C.Char <$> L.charLiteral,
    C.String . fromString <$> L.stringLiteral,
    C.Null <$ L.reserved "null",
    C.Undefined <$ L.reserved "undefined"
  ]
