module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement

data Statement =
  Skip | Assignment String Expr.T |
  Begin [Statement] | If Expr.T Statement Statement |
  While Expr.T Statement |
  Read String |
  Write Expr.T
    deriving Show


statement = skipStmt ! assignmentStmt ! beginStmt ! ifStmt ! whileStmt ! readStmt ! writeStmt

skipStmt = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip

assignmentStmt = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

beginStmt = accept "begin" -# (iter (parse #- spaces)) #- require "end" >-> buildBegin
buildBegin s = Begin s

ifStmt = accept "if" -# Expr.parse #- (require "then" #- spaces) # parse #- (spaces -# require "else" #- spaces) # parse >-> buildIf
buildIf ((e,s1),s2) = If e s1 s2


whileStmt = accept "while" -# Expr.parse #- (require "do" #- spaces) # parse >-> buildWhile 
buildWhile (e, s) = While e s

readStmt = accept "read" -# word #- require ";" >-> buildRead
buildRead v = Read v

writeStmt = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (Skip: stmts) dict input = exec stmts dict input 

exec (Assignment var value: stmts) dict input = 
    exec stmts (Dictionary.insert (var, Expr.value value dict) dict) input

exec (Begin stmt:stmts) dict input =
    exec (stmt++stmts) dict input

exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (While cond stmt: stmts) dict input =
    if(Expr.value cond dict)>0 
    then exec (stmt: (While cond stmt): stmts) dict input
    else exec stmts dict input

exec (Read var : stmts) dict (i:input) =
    exec stmts (Dictionary.insert(var, i) dict)  input

exec (Write val :stmts) dict input = 
    Expr.value val dict : exec stmts dict input


shw :: T -> String

shw (Skip) = "skip;\n"
shw (Assignment v e) = v++" := "++(Expr.toString e)++";\n"
shw (Begin stmts) = "begin\n" ++ (concatMap shw stmts)++"end\n"
shw (If e s1 s2) = "if " ++ (Expr.toString e) ++ " then\n" ++ (shw s1) ++"else\n" ++(shw s2)
shw (While e s) = "while " ++ (Expr.toString e) ++ " do\n" ++(shw s)
shw (Read v) = "read "++v++";\n"
shw (Write e) = "write "++(Expr.toString e)++";\n"

instance Parse Statement where
  parse = statement
  toString = shw
