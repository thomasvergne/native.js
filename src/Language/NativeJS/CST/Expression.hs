{-# LANGUAGE FlexibleInstances #-}

module Language.NativeJS.CST.Expression where

import qualified Data.Map                              as M
import           Language.NativeJS.CST.Modules.Literal (Literal)
import qualified Language.NativeJS.CST.Operators       as OP
import qualified Language.NativeJS.CST.Property        as P

data Expression
  = Literal Literal
  -- ^ literal
  | Array [Expression]
  -- ^ [ expressions ]
  | Object (M.Map P.Property Expression)
  -- ^ { property1: expression1, property2: expression2, ... }
  | This
  -- ^ this
  | Identifier Text
  -- ^ identifier
  | Property Expression Text
  -- ^ expression.identifier
  | Index Expression Expression
  -- ^ expression[expression]
  | Prefix OP.UnaryOp Expression
  -- ^ unaryOp expression
  | Binary OP.BinaryOp Expression Expression
  -- ^ expression binaryOp expression
  | Ternary Expression Expression Expression
  -- ^ expression ? expression : expression
  | Assignment OP.AssignmentOp LeftValue Expression
  -- ^ leftValue assignmentOp expression
  | Call Expression [Expression]
  -- ^ expression (arguments)
  | Lambda (Maybe Text) [Text] Statement
  -- ^ function [identifier] (arguments) { statements }
  deriving (Eq, Show)

data Statement
  = Block [Statement]
  -- ^ { statements }
  | Empty
  -- ^ ;
  | Expression Expression
  -- ^ expression;
  | If Expression Statement Statement
  | While Expression Statement
  -- ^ while (expression) { statement }
  | DoWhile Statement Expression
  -- ^ do { statement } while (expression)
  | For ForInit (Maybe Expression) (Maybe Expression) Statement
  -- ^ for (ForInit; Expression; Expression) { Statement[] }
  | ForIn LeftValue Expression Statement
  | Break (Maybe Text)
  -- ^ break [label]
  | Continue (Maybe Text)
  -- ^ continue [label]
  | Label Text Statement
  -- ^ label: statement
  | Switch Expression [SwitchCase]
  -- ^ switch (expression) { cases }
  | Try Statement (Maybe CatchClause) (Maybe Statement)
  | Throw Expression
  -- ^ throw expression
  | Return (Maybe Expression)
  -- ^ return [expression]
  | With Expression Statement
  -- ^ with (expression) { statement }
  | Declaration (M.Map Text (Maybe Expression))
  -- ^ var identifier [= expression]
  | Function Text [Text] Statement
  -- ^ function identifier (arguments) { statements }
  deriving (Eq, Show)

-- | ForInit is used in for statements
-- | for (ForInit; Expression; Expression) { Statement[] }
data ForInit
  = ForInitExpression Expression
  -- ^ expression
  | ForInitVar (M.Map Text (Maybe Expression))
  -- ^ var identifier [= expression]
  | ForInitEmpty
  -- ^ No initialization
  deriving (Eq, Show)

-- | SwitchCase is used in switch statements
-- | switch (expression) { SwitchCase[] }
data SwitchCase
  = SwitchCase Expression [Statement]
  -- ^ case expression: { statements }
  | SwitchDefault [Statement]
  -- ^ default: { statements }
  deriving (Eq, Show)

-- | CatchClause is used in try statements
-- | try { Statement[] } CatchClause [finally { Statement[] }]
data CatchClause =
  CatchClause Text Statement
  -- ^ catch (identifier) { statement }
  deriving (Eq, Show)

-- | LeftValue is used in assignment expressions
-- | LeftValue = Expression
data LeftValue
  = LeftValueIdentifier Text
  -- ^ Identifier
  | LeftValueProperty LeftValue Text
  -- ^ LeftValue.Property
  | LeftValueIndex LeftValue Expression
  -- ^ LeftValue[Index]
  deriving (Eq, Show)
