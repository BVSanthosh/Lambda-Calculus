module Lambda.Quote where
import Common
import Lambda.AST

{-This Haskell module contains functions for converting values in 
the lambda calculus to corresponding checkable and inferable terms.-}

{-PARTS OF THIS CODE WAS WRITTEN WITH THE HELP OF CHATGPT-}

quote0 :: Value -> CTerm
quote0 = quote 0
 
{-
    Converts a "Value" to a Checkable term
    This is mostly used for debugging and printing purposes.
-} 

quote :: Int -> Value -> CTerm
quote ii (VLam f) = 
    Lam (quote (ii + 1) (f (VNeutral (NFree (Local ii)))))

quote ii (VNeutral n) = 
    Inf (neutralQuote ii n)

{-
    Converts a Neutral Value into an Inferrable term.
    This is mostly used for debugging and printing purposes.
-}

neutralQuote :: Int -> Neutral -> ITerm
neutralQuote ii (NFree n)  = 
    case n of
        Global string ->
            Free n
        Local index ->
            Bound (ii - index - 1)
        _ -> error "Invalid Name information"
    
neutralQuote ii (NApp abs exp) = 
    neutralQuote ii abs :@: quote ii exp