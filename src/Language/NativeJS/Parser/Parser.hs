module Language.NativeJS.Parser.Parser where
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as E
import qualified Language.NativeJS.CST.Expression as C
import qualified Language.NativeJS.Parser.Lexer as L
import qualified Language.NativeJS.CST.Operators as C

import qualified Data.Functor as F
import qualified Language.NativeJS.Parser.Modules.Operator as OP
import qualified Language.NativeJS.Parser.Modules.Literal as LT
import qualified Language.NativeJS.Parser.Modules.Property as PP

import qualified Data.Map as M
import qualified Data.Foldable as F

type SourceFile = String

parseNative :: Monad m => SourceFile -> String -> m (Either P.ParseError [C.Statement])
parseNative = P.runParserT (L.whiteSpace *> P.many parseStatement <* P.eof) ()

makeUnaryOp :: Alternative f => f (a -> a) -> f (a -> a)
makeUnaryOp s = F.foldr1 (.) . reverse <$> some s

parseExpression :: Monad m => L.Parser m C.Expression
parseExpression = do
  table <- OP.operatorTable
  E.buildExpressionParser (newTable ++ table ++ ternTable) parseTerm

  where newTable = [[E.Postfix $ makeUnaryOp postfix]]
        ternTable = [[E.Postfix $ makeUnaryOp ternary]]
        ternary = do
          L.reservedOp "?"
          e1 <- parseExpression
          L.reservedOp ":"
          e2 <- parseExpression
          return $ \x -> C.Ternary x e1 e2

        postfix = functionCall <|> dotProperty <|> arrayProperty
        dotProperty = do
          L.reservedOp "."
          name <- L.identifier
          return $ \x -> C.Property x name
        arrayProperty = do
          index <- L.brackets parseExpression
          return $ \x -> C.Index x index
        functionCall = do
          args <- L.parens $ L.commaSep parseExpression
          return $ \x -> C.Call x args

parseTerm :: Monad m => L.Parser m C.Expression
parseTerm = P.choice [
    parseAssignment,
    parseVariable,
    parseLiteral,
    parseArray,
    parseObject,
    parseThis,
    parseLambda
  ]

parseVariable :: Monad m => L.Parser m C.Expression
parseVariable = L.lexeme $ L.identifier F.<&> C.Identifier

parseLiteral :: Monad m => L.Parser m C.Expression
parseLiteral = L.lexeme $ LT.parseLiteral F.<&> C.Literal

parseArray :: Monad m => L.Parser m C.Expression
parseArray = L.lexeme $ L.brackets (L.commaSep parseExpression) F.<&> C.Array

parseObject :: Monad m => L.Parser m C.Expression
parseObject = L.lexeme $ do
  properties <- L.braces . L.commaSep $ do
    name <- PP.parseProperty
    L.reservedOp ":"
    value <- parseExpression
    return (name, value)
  return $ C.Object $ M.fromList properties

parseThis :: Monad m => L.Parser m C.Expression
parseThis = L.lexeme $ L.reserved "this" F.$> C.This

parseAssignment :: Monad m => L.Parser m C.Expression
parseAssignment = L.lexeme $ do
  (left, op) <- P.try $ (,) <$> parseLeftValue <*> assignmentOp
  C.Assignment op left <$> parseExpression

parseLambda :: Monad m => L.Parser m C.Expression
parseLambda = L.lexeme $ do
  L.reserved "function"
  name <- P.optionMaybe L.identifier
  args <- L.parens $ L.commaSep L.identifier
  C.Lambda name args <$> parseStatement

parseStatement :: Monad m => L.Parser m C.Statement
parseStatement = P.choice [
    parseBlock,
    parseIf,
    parseWhile,
    parseFor,
    parseReturn,
    parseBreak,
    parseContinue,
    parseTry,
    parseThrow,
    parseSwitch,
    parseWith,
    parseForIn,
    parseLabel,
    parseFunction,
    parseDeclaration,
    L.lexeme $ C.Expression <$> parseExpression
  ] <* P.optionMaybe L.semi

parseBlock :: Monad m => L.Parser m C.Statement
parseBlock = L.lexeme $ L.braces (P.many $ L.lexeme parseStatement) F.<&> C.Block

parseIf :: Monad m => L.Parser m C.Statement
parseIf = L.lexeme $ do
  L.reserved "if"
  cond <- L.parens parseExpression
  then' <- parseStatement
  else' <- P.option (C.Block []) $ L.reserved "else" *> parseStatement
  return $ C.If cond then' else'

parseWhile :: Monad m => L.Parser m C.Statement
parseWhile = L.lexeme $ do
  L.reserved "while"
  cond <- L.parens parseExpression
  C.While cond <$> parseStatement

parseFor :: Monad m => L.Parser m C.Statement
parseFor = L.lexeme $ do
  L.reserved "for"
  (init', cond, step) <- L.parens $ do
    init' <- parseForInit
    _ <- L.semi
    cond <- P.optionMaybe parseExpression
    _ <- L.semi
    step <- P.optionMaybe parseExpression
    return (init', cond, step)
  C.For init' cond step <$> parseStatement

parseForIn :: Monad m => L.Parser m C.Statement
parseForIn = L.lexeme $ do
  L.reserved "for"
  (init', cond) <- L.parens $ do
    init' <- parseLeftValue
    _ <- L.reserved "in"
    cond <- parseExpression
    return (init', cond)
  C.ForIn init' cond <$> parseStatement

parseBreak :: Monad m => L.Parser m C.Statement
parseBreak = L.lexeme $ L.reserved "break" >> P.optionMaybe L.identifier F.<&> C.Break

parseContinue :: Monad m => L.Parser m C.Statement
parseContinue = L.lexeme $ L.reserved "continue" >> P.optionMaybe L.identifier F.<&> C.Continue

parseLabel :: Monad m => L.Parser m C.Statement
parseLabel = L.lexeme $ do
  name <- P.try $ L.identifier <* L.colon
  C.Label name <$> parseStatement

parseSwitch :: Monad m => L.Parser m C.Statement
parseSwitch = L.lexeme $ do
  L.reserved "switch"
  cond <- L.parens parseExpression
  cases <- L.braces $ P.many parseSwitchCase
  return $ C.Switch cond cases

parseSwitchCase :: Monad m => L.Parser m C.SwitchCase
parseSwitchCase = P.choice [
    parseSwitchCaseExpr,
    parseSwitchCaseDefault
  ]

parseSwitchCaseExpr :: Monad m => L.Parser m C.SwitchCase
parseSwitchCaseExpr = L.lexeme $ do
  cond <- L.reserved "case" *> parseExpression
  _ <- L.colon
  C.SwitchCase cond <$> P.many parseStatement

parseSwitchCaseDefault :: Monad m => L.Parser m C.SwitchCase
parseSwitchCaseDefault = L.lexeme $ do
  _ <- L.reserved "default"
  _ <- L.colon
  C.SwitchDefault <$> P.many parseStatement

parseForInit :: Monad m => L.Parser m C.ForInit
parseForInit = P.choice [
    parseForInitExpr,
    parseForInitVar,
    parseForInitEmpty
  ]

parseTry :: Monad m => L.Parser m C.Statement
parseTry = L.lexeme $ do
  L.reserved "try"
  try' <- parseStatement
  catch' <- P.optionMaybe parseCatchClause
  finally' <- P.optionMaybe $ L.reserved "finally" *> parseStatement
  return $ C.Try try' catch' finally'

parseThrow :: Monad m => L.Parser m C.Statement
parseThrow = L.lexeme $ do
  L.reserved "throw"
  C.Throw <$> parseExpression

parseReturn :: Monad m => L.Parser m C.Statement
parseReturn = L.lexeme $ do
  L.reserved "return"
  C.Return <$> P.optionMaybe parseExpression

parseWith :: Monad m => L.Parser m C.Statement
parseWith = L.lexeme $ do
  L.reserved "with"
  cond <- L.parens parseExpression
  C.With cond <$> parseStatement

parseDeclaration :: Monad m => L.Parser m C.Statement
parseDeclaration = L.lexeme $ do
  L.reserved "var"
  vars <- L.commaSep $ do
    name <- L.identifier
    init' <- P.optionMaybe $ L.reservedOp "=" *> parseExpression
    return (name, init')
  return $ C.Declaration $ M.fromList vars

parseFunction :: Monad m => L.Parser m C.Statement
parseFunction = L.lexeme $ do
  L.reserved "function"
  name <- L.identifier
  args <- L.parens $ L.commaSep L.identifier
  C.Function name args <$> parseStatement

parseCatchClause :: Monad m => L.Parser m C.CatchClause
parseCatchClause = L.lexeme $ do
  L.reserved "catch"
  name <- L.parens L.identifier
  C.CatchClause name <$> parseStatement

parseForInitExpr :: Monad m => L.Parser m C.ForInit
parseForInitExpr = C.ForInitExpression <$> parseExpression

parseForInitVar :: Monad m => L.Parser m C.ForInit
parseForInitVar = do
  L.reserved "var"
  vars <- L.commaSep $ do
    name <- L.identifier
    init' <- P.optionMaybe $ L.reservedOp "=" *> parseExpression
    return (name, init')
  return $ C.ForInitVar $ M.fromList vars

parseForInitEmpty :: Monad m => L.Parser m C.ForInit
parseForInitEmpty = return C.ForInitEmpty

parseLeftValue :: Monad m => L.Parser m C.LeftValue
parseLeftValue = E.buildExpressionParser table parseLeftValueIdentifier
  where table = [[
            E.Postfix $ makeUnaryOp postfix
          ]]
        postfix = member <|> index
        member = do
          L.reservedOp "."
          prop <- L.identifier
          return $ \expr -> C.LeftValueProperty expr prop
        index = do
          e <- L.brackets parseExpression
          return $ \expr -> C.LeftValueIndex expr e

parseLeftValueIdentifier :: Monad m => L.Parser m C.LeftValue
parseLeftValueIdentifier = L.lexeme $ L.identifier F.<&> C.LeftValueIdentifier

parseLeftValueMember :: Monad m => L.Parser m C.LeftValue
parseLeftValueMember = L.lexeme $ do
  obj <- parseLeftValue
  _ <- L.dot
  C.LeftValueProperty obj <$> L.identifier

parseLeftValueIndex :: Monad m => L.Parser m C.LeftValue
parseLeftValueIndex = L.lexeme $ do
  obj <- parseLeftValue
  e <- L.brackets parseExpression
  return $ C.LeftValueIndex obj e

assignmentOp :: Monad m => L.Parser m C.AssignmentOp
assignmentOp = L.lexeme $ P.choice [
    L.reservedOp "=" F.$> C.AssignmentOpAssign,
    L.reservedOp "+=" F.$> C.AssignmentOpPlus,
    L.reservedOp "-=" F.$> C.AssignmentOpMinus,
    L.reservedOp "*=" F.$> C.AssignmentOpMultiply,
    L.reservedOp "/=" F.$> C.AssignmentOpDivide,
    L.reservedOp "%=" F.$> C.AssignmentOpModulo,
    L.reservedOp "<<=" F.$> C.AssignmentOpLeftShift,
    L.reservedOp ">>=" F.$> C.AssignmentOpRightShift,
    L.reservedOp ">>>=" F.$> C.AssignmentOpUnsignedRightShift,
    L.reservedOp "&=" F.$> C.AssignmentOpBitwiseAnd,
    L.reservedOp "^=" F.$> C.AssignmentOpBitwiseXor,
    L.reservedOp "|=" F.$> C.AssignmentOpBitwiseOr
  ]