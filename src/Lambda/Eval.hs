module Lambda.Eval where
import Common
import Lambda.AST

{-This Haskell module, Lambda.Eval, contains placeholders for two functions 
that perform evaluation of lambda calculus terms.-}

{-
    Evaluates an (inferrable) term to a normal form. 
    The result is a Value type. 
-}

iEval :: ITerm -> (NameEnv Value, Env) -> Value
iEval (Ann cterm t) d =  
    cEval cterm d 

iEval (Bound index) d = 
    (snd d) !! index

iEval (Free name) d = 
    case name of
        Global string ->
            case lookup name (fst d) of
                Just value -> value
                Nothing -> VNeutral (NFree name)
        _ -> error "Unexpected value"

iEval (iterm :@: cterm) d = 
    case iEval iterm d of
        VLam f -> f (cEval cterm d)
        VNeutral n -> VNeutral (NApp n (cEval cterm d))
        _ -> error "Unexpected evaluation"
   
{-
    Evaluates a (checkable) term to a normal form.
    The result is a Value type.
-}

cEval :: CTerm -> (NameEnv Value, Env) -> Value
cEval (Inf iterm) d  =  
    iEval iterm d 

cEval (Lam cterm) d  =  
    VLam (\arg -> cEval cterm (fst d, arg : snd d))