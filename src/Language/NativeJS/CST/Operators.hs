module Language.NativeJS.CST.Operators where
data UnaryOp
  = UnaryOpDelete                  
  -- ^ delete expression
  | UnaryOpVoid                    
  -- ^ void expression
  | UnaryOpTypeof                  
  -- ^ typeof expression
  | UnaryOpPlus                    
  -- ^ +expression
  | UnaryOpMinus                   
  -- ^ -expression
  | UnaryOpBitwiseNot              
  -- ^ ~expression
  | UnaryOpLogicalNot              
  -- ^ !expression

  | PrefixInc                      
  -- ^ ++expression
  | PrefixDec                      
  -- ^ --expression

  | PostfixInc                     
  -- ^ expression++
  | PostfixDec                     
  -- ^ expression--
  deriving (Eq, Show)

data BinaryOp
  = BinaryOpMultiply               
  -- ^ expression * expression
  | BinaryOpDivide                 
  -- ^ expression / expression
  | BinaryOpModulo                 
  -- ^ expression % expression
  | BinaryOpPlus                   
  -- ^ expression + expression
  | BinaryOpMinus                  
  -- ^ expression - expression
  | BinaryOpLeftShift              
  -- ^ expression << expression
  | BinaryOpRightShift             
  -- ^ expression >> expression
  | BinaryOpUnsignedRightShift     
  -- ^ expression >>> expression
  | BinaryOpLessThan               
  -- ^ expression < expression
  | BinaryOpGreaterThan            
  -- ^ expression > expression
  | BinaryOpLessThanOrEqual        
  -- ^ expression <= expression
  | BinaryOpGreaterThanOrEqual     
  -- ^ expression >= expression
  | BinaryOpIn                     
  -- ^ expression in expression
  | BinaryOpInstanceOf             
  -- ^ expression instanceof expression
  | BinaryOpEqual                  
  -- ^ expression == expression
  | BinaryOpNotEqual               
  -- ^ expression != expression
  | BinaryOpStrictEqual            
  -- ^ expression === expression
  | BinaryOpStrictNotEqual         
  -- ^ expression !== expression
  | BinaryOpBitwiseAnd             
  -- ^ expression & expression
  | BinaryOpBitwiseXor             
  -- ^ expression ^ expression
  | BinaryOpBitwiseOr              
  -- ^ expression | expression
  | BinaryOpLogicalAnd             
  -- ^ expression && expression
  | BinaryOpLogicalOr              
  -- ^ expression || expression
  deriving (Eq, Show)

data AssignmentOp
  = AssignmentOpAssign             
  -- ^ expression = expression
  | AssignmentOpMultiply           
  -- ^ expression *= expression
  | AssignmentOpDivide             
  -- ^ expression /= expression
  | AssignmentOpModulo             
  -- ^ expression %= expression
  | AssignmentOpPlus               
  -- ^ expression += expression
  | AssignmentOpMinus              
  -- ^ expression -= expression
  | AssignmentOpLeftShift          
  -- ^ expression <<= expression
  | AssignmentOpRightShift         
  -- ^ expression >>= expression
  | AssignmentOpUnsignedRightShift 
  -- ^ expression >>>= expression
  | AssignmentOpBitwiseAnd         
  -- ^ expression &= expression
  | AssignmentOpBitwiseXor         
  -- ^ expression ^= expression
  | AssignmentOpBitwiseOr          
  -- ^ expression |= expression
  deriving (Eq, Show)