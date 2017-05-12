module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving Show
instance Parse T where
  parse = Statement.parse
  toString = error "lul"
             
exec (Program list) = Statement.exec list Dictionary.empty 
