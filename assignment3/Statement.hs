module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    Skip |
    Begin [Statement]
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

conditional = accept "if" -# Expr.parse #- require "then" # parse #- 
            require "else" # parse >-> buildCon
buildCon ((b, s1), s2) = If b s1 s2

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, s) = While e s

readVar = accept "read" -# word #- require ";" >-> buildRead
buildRead v = Read v

writeExpr = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e

skip = accept "skip" -# require ";" >-> buildSkip
buildSkip _ = Skip

begin = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin s = Begin s

comment' = accept "--" -# comment >-> buildSkip

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (While cond doStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (doStmts : (While cond doStmts) : stmts) dict input
    else exec stmts dict input

exec (Assignment var expr: stmts) dic input =
   exec stmts (Dictionary.insert (var, Expr.value expr dic) dic) input

exec (Read var: stmts) dic (x:xs) = 
    exec stmts (Dictionary.insert (var, x) dic) xs

exec (Write expr: stmts) dic input =
    (Expr.value expr dic): exec stmts dic input

exec (Skip : stmts) dic input = 
    exec stmts dic input

exec (Begin doStmts : stmts) dic input =
    exec (doStmts ++ stmts) dic input

exec [] _ _ = []

instance Parse Statement where
  parse = assignment ! conditional ! while ! readVar ! writeExpr !
            skip ! begin ! comment'
  toString = toString' 0

toString' :: Int -> Statement -> String
toString' i (Assignment s e) = indent i ++ s ++ " := " ++ Expr.toString e ++ ";\n"

toString' i (If e s1 s2) = indent i ++ "if " ++ Expr.toString e ++ " then\n" ++ toString' (i+1) s1 ++ indent i ++ "else\n" ++ toString' (i+1) s2

toString' i (While e s) = indent i ++ "while " ++ Expr.toString e ++ " do\n" ++ toString' (i+1) s

toString' i (Read s) = indent i ++ "read " ++ s ++ ";\n"

toString' i (Write e) = indent i ++ "write " ++ Expr.toString e ++ ";\n"

toString' i (Skip) = indent i ++ "skip;\n"

toString' i (Begin stmts) = indent i ++ "begin\n" ++ concat (map (\stmt -> toString' (i+1) stmt) stmts) ++ indent i ++ "end\n"

indent :: Int -> String
indent i = concat $ replicate i "\t"
