module Common where
import Text.PrettyPrint.HughesPJ hiding (parens)
import qualified Text.PrettyPrint.HughesPJ as PP

{-This module provides essential data types and functions that are
 used throughout the lambda calculus interpreter, offering a foundation 
 for representing names, types, environments, evaluation statements, and 
 utility functions for formatting-}

-- represents global names, local variables and quoted names
data Name
   =  Global  String
   |  Local   Int
   |  Quote   Int
  deriving (Show, Eq)

-- represents free variables and functions
data Type
   =  TFree  Name
   |  Fun    Type Type
  deriving (Show, Eq) 

-- represents a result that can either be a value of type a or a string (error message)
type Result a = Either String a

-- represents an environment where names are associated with values of type v
type NameEnv v = [(Name, v)]

-- represents all the different programmming commands
data Stmt i tinf = Let String i           --  let x = t
                   | Assume [(String,tinf)] --  assume x :: t, assume x :: *
                   | Eval i
                   | PutStrLn String        --  lhs2TeX hacking, allow to print "magic" string
                   | Out String             --  more lhs2TeX hacking, allow to print to files
    deriving (Show)

-- 
parensIf :: Bool -> Doc -> Doc
parensIf True  = PP.parens
parensIf False = id

-- represents a list of variable names 
vars :: [String]
vars = [ c : n | n <- "" : map show [1..], c <- ['x','y','z'] ++ ['a'..'w'] ]