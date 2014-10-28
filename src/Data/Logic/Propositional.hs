module Data.Logic.Propositional where

import Data.Function (on)

type Variable = Char

data Expr
    = Literal     Variable
    | Negation    Expr
    | Conjunction Expr Expr
    | Disjunction Expr Expr
    | Implication Expr Expr
    deriving Eq

instance Show Expr where
    show formula = case formula of
            (Literal x)       -> [x]
            (Negation x)      -> "¬" ++ parens x
            (Conjunction x y) -> showBinary "∧" x y
            (Disjunction x y) -> showBinary "∨" x y
            (Implication x y) -> showBinary "→" x y
        where
            showBinary op x y = parens x ++ " " ++ op ++ " " ++ parens y

            parens (Literal x) = [x]
            parens x           = "(" ++ show x ++ ")"

conjunctiveNormalForm :: Expr -> Expr
conjunctiveNormalForm = conjunctiveNormalForm' . negationNormalForm

conjunctiveNormalForm' :: Expr -> Expr
conjunctiveNormalForm' = id

negationNormalForm :: Expr -> Expr
negationNormalForm = negationNormalForm' . implicationFree

negationNormalForm' :: Expr -> Expr
negationNormalForm' (Literal x)       = Literal x
negationNormalForm' (Conjunction x y) =
    (Conjunction `on` negationNormalForm') x y
negationNormalForm' (Disjunction x y) =
    (Disjunction `on` negationNormalForm') x y
negationNormalForm' (Negation (Literal x)) = Negation (Literal x)
negationNormalForm' (Negation (Negation x)) = negationNormalForm' x
negationNormalForm' (Negation (Conjunction x y)) =
    (Disjunction `on` (negationNormalForm' . Negation)) x y
negationNormalForm' (Negation (Disjunction x y)) =
    (Conjunction `on` (negationNormalForm' . Negation)) x y
negationNormalForm' _ = error "negationNormalForm': incorrect expression"

implicationFree :: Expr -> Expr
implicationFree = id
