module Data.Logic.Propositional
    ( Expr(..)
    , Variable
    , toCNF
    ) where

import Data.Function (on)

type Variable = Char

data Expr
    = Literal     Variable
    | Negation    Expr
    | Conjunction Expr Expr
    | Disjunction Expr Expr
    | Implication Expr Expr
    deriving Eq

neg :: Expr -> Expr
neg = Negation

conj :: Expr -> Expr -> Expr
conj = Conjunction

disj :: Expr -> Expr -> Expr
disj = Disjunction

instance Show Expr where
    show expr = case expr of
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
    where
        toCNF' (Conjunction x y) = (conj `on` toCNF') x y
        toCNF' (Disjunction x y) = (dist `on` toCNF') x y
        toCNF' x                 = x

        dist (Conjunction x1 x2) y = (x1 `dist` y) `conj` (x2 `dist` y)
        dist x (Conjunction y1 y2) = (x `dist` y1) `conj` (x `dist` y2)
        dist x y                   = x `disj` y

toNNF :: Expr -> Expr
toNNF x@(Literal _) = x
toNNF (Conjunction x y) = (conj `on` toNNF) x y
toNNF (Disjunction x y) = (disj `on` toNNF) x y
toNNF (Implication x y) = (disj `on` toNNF) (neg x) y
toNNF (Negation (Literal x)) = Negation (Literal x)
toNNF (Negation (Negation x)) = toNNF x
toNNF (Negation (Conjunction x y)) = (disj `on` (toNNF . neg)) x y
toNNF (Negation (Disjunction x y)) = (conj `on` (toNNF . neg)) x y
toNNF (Negation (Implication x y)) = (conj `on` toNNF) x (neg y)
