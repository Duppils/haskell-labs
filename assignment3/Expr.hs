module Expr(Expr, T, parse, fromString, value, toString) where

{-
   An expression of type Expr is a representation of an arithmetic expression 
   with integer constants and variables. A variable is a string of upper- 
   and lower case letters. The following functions are exported
   
   parse :: Parser Expr
   fromString :: String -> Expr
   toString :: Expr -> String
   value :: Expr -> Dictionary.T String Int -> Int
   
   parse is a parser for expressions as defined by the module Parser.
   It is suitable for use in parsers for languages containing expressions
   as a sublanguage.
   
   fromString expects its argument to contain an expression and returns the 
   corresponding Expr. 
  
   toString converts an expression to a string without unneccessary 
   parentheses and such that fromString (toString e) = e.
  
   value e env evaluates e in an environment env that is represented by a
   Dictionary.T Int.  
-}
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary

data Expr = Num Integer | Var String | Add Expr Expr 
       | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Exp Expr Expr
         deriving Show

type T = Expr

var, num, factor, term, expTerm, expr :: Parser Expr

term', expr', expTerm' :: Expr -> Parser Expr

var = word >-> Var

num = number >-> Num

mulOp = lit '*' >-> (\ _ -> Mul) !
        lit '/' >-> (\ _ -> Div)

addOp = lit '+' >-> (\ _ -> Add) !
        lit '-' >-> (\ _ -> Sub)

bldOp e (oper,e') = oper e e'

expOp = lit '^' >-> (\ _ -> Exp)

factor = num !
         var !
         lit '(' -# expr #- lit ')' !
         err "illegal factor"
            
expTerm' e = expOp # factor >-> bldOp e #> expTerm' ! return e
expTerm = factor #> expTerm'

term' e = mulOp # expTerm >-> bldOp e #> term' ! return e
term = expTerm #> term'
       
expr' e = addOp # term >-> bldOp e #> expr' ! return e
expr = term #> expr'

parens cond str = if cond then "(" ++ str ++ ")" else str

shw :: Int -> Expr -> String
shw prec (Num n) = show n
shw prec (Var v) = v
shw prec (Add t u) = parens (prec>5) (shw 5 t ++ "+" ++ shw 5 u)
shw prec (Sub t u) = parens (prec>5) (shw 5 t ++ "-" ++ shw 6 u)
shw prec (Mul t u) = parens (prec>6) (shw 6 t ++ "*" ++ shw 6 u)
shw prec (Div t u) = parens (prec>6) (shw 6 t ++ "/" ++ shw 7 u)
shw prec (Exp t u) = parens (prec>7) (shw 7 t ++ "^" ++ shw 7 u)

value :: Expr -> Dictionary.T String Integer -> Integer
value (Num n) _ = n
value (Var s) dic = case (Dictionary.lookup s dic) of 
                    Nothing -> error ("Variable " ++ s ++ " not found!")
                    Just(i) -> i
value (Add e1 e2) dic = (value e1 dic) + (value e2 dic)
value (Sub e1 e2) dic = (value e1 dic) - (value e2 dic)
value (Mul e1 e2) dic = (value e1 dic) * (value e2 dic)
value (Div e1 e2) dic = case denominator of
                        0 -> error "Division by zero!"
                        i -> (value e1 dic) `div` denominator
                        where denominator = value e2 dic
value (Exp e1 e2) dic = (value e1 dic) ^ (value e2 dic)

instance Parse Expr where
    parse = expr
    toString = shw 0
