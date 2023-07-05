module Language.NativeJS.Parser.Modules.Operator where
import qualified Text.Parsec.Expr as E
import qualified Language.NativeJS.CST.Expression as C
import qualified Language.NativeJS.Parser.Lexer as L
import qualified Language.NativeJS.CST.Operators as C

operatorTable :: Monad m => L.Parser m [[E.Operator String () m C.Expression]]
operatorTable = do
  return [
      [ binary "||" (C.Binary C.BinaryOpLogicalOr) E.AssocLeft,
        binary "&&" (C.Binary C.BinaryOpLogicalAnd) E.AssocLeft ],

      [ binary "|" (C.Binary C.BinaryOpBitwiseOr) E.AssocLeft,
        binary "^" (C.Binary C.BinaryOpBitwiseXor) E.AssocLeft,
        binary "&" (C.Binary C.BinaryOpBitwiseAnd) E.AssocLeft ],

      [ binary "!==" (C.Binary C.BinaryOpStrictNotEqual) E.AssocLeft,
        binary "!=" (C.Binary C.BinaryOpNotEqual) E.AssocLeft,
        binary "===" (C.Binary C.BinaryOpStrictEqual) E.AssocLeft,
        binary "==" (C.Binary C.BinaryOpEqual) E.AssocLeft ],

      [ binary ">=" (C.Binary C.BinaryOpGreaterThanOrEqual) E.AssocLeft,
        binary ">" (C.Binary C.BinaryOpGreaterThan) E.AssocLeft,
        binary "<=" (C.Binary C.BinaryOpLessThanOrEqual) E.AssocLeft,
        binary "<" (C.Binary C.BinaryOpLessThan) E.AssocLeft ],

      [ binary "instanceof" (C.Binary C.BinaryOpInstanceOf) E.AssocLeft,
        binary "in" (C.Binary C.BinaryOpIn) E.AssocLeft ],

      [ binary ">>>" (C.Binary C.BinaryOpUnsignedRightShift) E.AssocLeft,
        binary ">>" (C.Binary C.BinaryOpRightShift) E.AssocLeft,
        binary "<<" (C.Binary C.BinaryOpLeftShift) E.AssocLeft ],
      
      [ binary "+" (C.Binary C.BinaryOpPlus) E.AssocLeft,
        binary "-" (C.Binary C.BinaryOpMinus) E.AssocLeft ],

      [ binary "%" (C.Binary C.BinaryOpModulo) E.AssocLeft,
        binary "/" (C.Binary C.BinaryOpDivide) E.AssocLeft,
        binary "*" (C.Binary C.BinaryOpMultiply) E.AssocLeft ],

      [ prefix "delete" (C.Prefix C.UnaryOpDelete),
        prefix "void" (C.Prefix C.UnaryOpVoid),
        prefix "typeof" (C.Prefix C.UnaryOpTypeof),
        prefix "-" (C.Prefix C.UnaryOpMinus),
        prefix "+" (C.Prefix C.UnaryOpPlus) ],
      
      [ prefix "~" (C.Prefix C.UnaryOpBitwiseNot),
        prefix "!" (C.Prefix C.UnaryOpLogicalNot),
        prefix "--" (C.Prefix C.PrefixDec),
        prefix "++" (C.Prefix C.PrefixInc) ],

      [ postfix "--" (C.Prefix C.PostfixDec),
        postfix "++" (C.Prefix C.PostfixInc) ] 
    ]

binary :: Monad m => String -> (a -> a -> a) -> E.Assoc -> E.Operator String () m a
binary name f = E.Infix (do
  L.reservedOp name
  return $ \x y -> f x y) 

prefix :: Monad m => String -> (a -> a) -> E.Operator String () m a
prefix name f = E.Prefix (do
  L.reservedOp name
  return $ \x -> f x)

postfix :: Monad m => String -> (a -> a) -> E.Operator String () m a
postfix name f = E.Postfix (do
  L.reservedOp name
  return $ \x -> f x)