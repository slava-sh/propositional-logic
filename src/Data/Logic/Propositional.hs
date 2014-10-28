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
toCNF' x@(Literal _)            = x
toCNF' x@(Negation (Literal _)) = x
toCNF' (Conjunction x y)        = (conj `on` toCNF') x y
toCNF' (Disjunction x y)        = (distributeDisj `on` toCNF') x y
toCNF' _                        = error "toCNF': incorrect expression"

distributeDisj :: Expr -> Expr -> Expr
distributeDisj (Conjunction x1 x2) y =
    distributeDisj x1 y `conj` distributeDisj x2 y
distributeDisj x (Conjunction y1 y2) =
    distributeDisj x y1 `conj` distributeDisj x y2
distributeDisj x y = x `disj` y

toNNF :: Expr -> Expr
toNNF = toNNF' . implicationFree

toNNF' :: Expr -> Expr
toNNF' x@(Literal _) = x
toNNF' (Conjunction x y) = (conj `on` toNNF') x y
toNNF' (Disjunction x y) = (disj `on` toNNF') x y
toNNF' (Negation (Literal x)) = Negation (Literal x)
toNNF' (Negation (Negation x)) = toNNF' x
toNNF' (Negation (Conjunction x y)) = (disj `on` (toNNF' . neg)) x y
toNNF' (Negation (Disjunction x y)) = (conj `on` (toNNF' . neg)) x y
toNNF' _ = error "toNNF': incorrect expression"

implicationFree :: Expr -> Expr
implicationFree x@(Literal _)     = x
implicationFree (Negation x)      = neg (implicationFree x)
implicationFree (Conjunction x y) = (conj `on` implicationFree) x y
implicationFree (Disjunction x y) = (disj `on` implicationFree) x y
implicationFree (Implication x y) =
    (neg (implicationFree x)) `disj` (implicationFree y)
