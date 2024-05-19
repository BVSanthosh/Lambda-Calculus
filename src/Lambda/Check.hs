module Lambda.Check where
import Control.Monad.Except
import Lambda.AST
import Common

{-This Haskell module provides functions for type
checking in the context of a simply typed lambda calculus.-}

{-
  cKind checks a type can be formulated or is a type (or "kind")
  It returns Result () if the type is well formulated, or an error message otherwise.
-}

cKind :: Context -> Type -> Kind -> Result ()
cKind g (TFree x) Star =
  case lookup x g of
    Just (HasKind t) -> return ()
    _ -> throwError "Undefined type" 

{-
  A simple wrapper function for iType, starting with 0 bindings
-}

iType0 :: Context -> ITerm -> Result Type
iType0 = iType 0

{-
   iType performs type "inference", where a type is inferred from a context 
      -- It returns Result Type, where Type is the inferred Type, otherwise an error message.
   Result is defined in Common.hs as Either an error or a result. 
-}

iType :: Int -> Context -> ITerm -> Result Type
iType ii g (Ann cterm t)  =
  do
    cType ii g cterm t
    return t

iType ii g (Free name) = 
  case name of 
    Global string ->
      case lookup (Global string) g of
        Just (HasType t) -> return t
        _ -> throwError "Type not in Context" 
     
iType ii g (iterm :@: cterm) = 
  do
    iType ii g iterm
    case cterm of
      (Inf innerIterm) ->
        iType ii g innerIterm
      _ -> throwError "Invalid CTerm information"
    
{-
   cType performs type "checking"
     it returns Result () if the type is correct (or "checks"), 
             otherwise we return an error message.
     Result is defined in Common.hs as Either an error messsage or a result. 
-}

cType :: Int -> Context -> CTerm -> Type -> Result ()
cType ii g (Inf iterm) t = 
  case t of
    TFree name ->
      do
        typeInf <- iType ii g iterm
        if typeInf == t
          then return () 
          else throwError "Undefined type"   

cType ii g cterm (Fun ty1 ty2) =
  do
    case ty1 of
      TFree t1 ->
          cKind g ty1 Star
      _ -> 
        cType ii g cterm ty1
    case ty2 of
      TFree t2 ->
        cKind g ty2 Star
      _ -> 
        cType ii g cterm ty2

cType ii g (Lam cterm) t =
  cType ii g cterm t