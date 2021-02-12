import Data.List
import Data.Maybe

type Symb = String 
infixl 2 :@
infix 1 `alphaEq`

data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
          deriving (Eq, Read, Show)
          
freeVars :: Expr -> [Symb]
freeVars (Var s)    = [s]
freeVars (f :@ g)   = union (freeVars f) (freeVars g)
freeVars (Lam s f)  = delete s (freeVars f)

subst :: Symb -> Expr -> Expr -> Expr
subst s ex (Var re)
    | s == re    = ex
    | otherwise  = Var re
subst s ex (f :@ g) = (subst s ex f) :@ (subst s ex g)
subst s ex (Lam re f)
    | s == re                       = Lam s f
    | not (isFree re (freeVars ex)) = Lam re (subst s ex f)
    | isFree re (freeVars ex)       = subst s ex (Lam (repl re) (subst re (Var (repl re)) f))
        where repl x = x ++ "'"

isFree :: Symb -> [Symb] -> Bool
isFree s xs
    | length xs == 0  = False
    | s == head xs    = True
    | otherwise       = isFree s $ tail xs

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var a) (Var b)     = compare a b == EQ
alphaEq (Lam x f) (Lam y g) 
    | x == y    = alphaEq f g
    | otherwise = (alphaEq (Lam x f) $ Lam x (subst y (Var x) g)) && (alphaEq (Lam y g) $ Lam y (subst x (Var y) f)) 
alphaEq (f :@ g) (h :@ l)   = (alphaEq f h) && (alphaEq g l)
alphaEq _ _                 = False

reduceOnce :: Expr -> Maybe Expr
reduceOnce (Var x)          = Nothing
reduceOnce ((Lam x f) :@ g) = Just $ subst x g f
reduceOnce (Lam x f) 
    | red == Nothing = Nothing 
    | otherwise      = Just (Lam x $ fromJust red)
        where 
            red = reduceOnce f
reduceOnce (f :@ g) 
    | redf == Nothing && redg == Nothing = Nothing
    | redf == Nothing                    = Just $ f :@ fromJust redg
    | redg == Nothing                    = Just $ fromJust redf :@ g
    | otherwise                          = Just $ fromJust redf :@ g
        where
            redf = reduceOnce f
            redg = reduceOnce g
reduceOnce _                = Nothing

nf :: Expr -> Expr
nf f 
    | red == Nothing = f
    | otherwise = nf (fromJust red)
        where
            red = reduceOnce f

infix 1 `betaEq`

betaEq :: Expr -> Expr -> Bool 
betaEq f g = alphaEq (nf f) $ nf g