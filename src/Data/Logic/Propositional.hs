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

toCNF :: Expr -> Expr
toCNF = toCNF' . toNNF

toCNF' :: Expr -> Expr
toCNF' = id

toNNF :: Expr -> Expr
toNNF = toNNF' . implicationFree

toNNF' :: Expr -> Expr
toNNF' (Literal x)       = Literal x
toNNF' (Conjunction x y) =
    (Conjunction `on` toNNF') x y
toNNF' (Disjunction x y) =
    (Disjunction `on` toNNF') x y
toNNF' (Negation (Literal x)) = Negation (Literal x)
toNNF' (Negation (Negation x)) = toNNF' x
toNNF' (Negation (Conjunction x y)) =
    (Disjunction `on` (toNNF' . Negation)) x y
toNNF' (Negation (Disjunction x y)) =
    (Conjunction `on` (toNNF' . Negation)) x y
toNNF' _ = error "toNNF': incorrect expression"

implicationFree :: Expr -> Expr
implicationFree (Literal x)       = Literal x
implicationFree (Negation x)      = Negation (implicationFree x)
implicationFree (Conjunction x y) = (Conjunction `on` implicationFree) x y
implicationFree (Disjunction x y) = (Disjunction `on` implicationFree) x y
implicationFree (Implication x y) =
    Disjunction (Negation (implicationFree x)) (implicationFree y)
