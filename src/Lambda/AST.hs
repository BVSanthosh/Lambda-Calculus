module Lambda.AST where
import Common

{-This module defines the abstract syntax tree (AST) for the simply typed
lambda calculus.-}

-- inferable term
data ITerm
   =  Ann    CTerm Type
   |  Bound  Int      -- The binding position of variables. (\x . x) = Bound 0. (\x.\y . x y) = (Bound 1) (Bound 0)
   |  Free   Name     -- Free variables need to be recorded in an "environment"
   |  ITerm :@: CTerm -- Applications
  deriving (Show, Eq)

-- checkable term
data CTerm
   =  Inf  ITerm
   |  Lam  CTerm
  deriving (Show, Eq)

-- Value and Neutral represents a normal form, Normal forms can be lambdas (functions), variables or applications.
data Value
   =  VLam      (Value -> Value)
   |  VNeutral  Neutral

data Neutral
   =  NFree  Name
   |  NApp   Neutral Value

-- represents the type of types
data Kind = Star
  deriving (Show)

-- provides additional information about terms during type checking
data Info
   =  HasKind  Kind
   |  HasType  Type 
  deriving (Show)

-- list of name-info pairs, used during type checking to store variable names and their associated type or kind information
type Context = [(Name, Info)]

-- a list of values, representing the environment during evaluation
type Env = [Value]