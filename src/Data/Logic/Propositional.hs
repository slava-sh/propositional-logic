module Data.Logic.Propositional
    ( Expr(..)
    , Atom
    , toCNF
    , toNNF
    ) where

import Data.Function (on)

type Atom = Char

data Expr
    = Var Atom
    | Neg Expr
    | Expr :/\ Expr
    | Expr :\/ Expr
    | Expr :-> Expr
    deriving Eq

instance Show Expr where
    show expr = case expr of
            (Var x)   -> [x]
            (Neg x)   -> "¬" ++ parens x
            (x :/\ y) -> showBinary "∧" x y
            (x :\/ y) -> showBinary "∨" x y
            (x :-> y) -> showBinary "→" x y
        where
            showBinary op x y = parens x ++ " " ++ op ++ " " ++ parens y

            parens (Var x) = [x]
            parens x       = "(" ++ show x ++ ")"

toCNF :: Expr -> Expr
toCNF = toCNF' . toNNF
    where
        toCNF' (x :/\ y) = ((:/\) `on` toCNF') x y
        toCNF' (x :\/ y) = (dist  `on` toCNF') x y
        toCNF' x         = x

        dist (x1 :/\ x2) y = (x1 `dist` y) :/\ (x2 `dist` y)
        dist x (y1 :/\ y2) = (x `dist` y1) :/\ (x `dist` y2)
        dist x y           = x :\/ y

toNNF :: Expr -> Expr
toNNF x@(Var _)       = x
toNNF x@(Neg (Var _)) = x
toNNF (x :/\ y)       = ((:/\) `on` toNNF) x y
toNNF (x :\/ y)       = ((:\/) `on` toNNF) x y
toNNF (x :-> y)       = ((:\/) `on` toNNF) (Neg x) y
toNNF (Neg (x :-> y)) = ((:/\) `on` toNNF) x (Neg y)
toNNF (Neg (x :/\ y)) = ((:\/) `on` (toNNF . Neg)) x y
toNNF (Neg (x :\/ y)) = ((:/\) `on` (toNNF . Neg)) x y
toNNF (Neg (Neg x))   = toNNF x
