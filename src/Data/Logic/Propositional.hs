module Data.Logic.Propositional
  ( Expr(..)
  , Atom
  , toCNF
  , toNNF
  ) where

type Atom = Char

data Expr
  = Var Atom
  | Neg Expr
  | Expr :/\ Expr
  | Expr :\/ Expr
  | Expr :-> Expr
  deriving Eq

infixl 3 :/\
infixl 2 :\/
infixr 1 :->

instance Show Expr where
  showsPrec p expr = case expr of
      (Var x)   -> showChar x
      (Neg x)   -> showChar '¬' . showsPrec 4 x
      (x :/\ y) -> showBinary " ∧ " 3 x y
      (x :\/ y) -> showBinary " ∨ " 2 x y
      (x :-> y) -> showBinary " → " 1 x y
    where
      showBinary op p' x y =
        showParen (p' < p) $ showsPrec p' x . showString op . showsPrec p' y

toCNF :: Expr -> Expr
toCNF = go . toNNF
  where
    go (x :/\ y) = go x :/\ go y
    go (x :\/ y) = go x `dist` go y
    go x         = x

    dist (x1 :/\ x2) y = (x1 `dist` y) :/\ (x2 `dist` y)
    dist x (y1 :/\ y2) = (x `dist` y1) :/\ (x `dist` y2)
    dist x y           = x :\/ y

toNNF :: Expr -> Expr
toNNF x@(Var _)       = x
toNNF x@(Neg (Var _)) = x
toNNF (Neg (Neg x))   = toNNF x
toNNF (x :/\ y)       = toNNF x       :/\ toNNF y
toNNF (x :\/ y)       = toNNF x       :\/ toNNF y
toNNF (x :-> y)       = toNNF (Neg x) :\/ toNNF y
toNNF (Neg (x :/\ y)) = toNNF (Neg x) :\/ toNNF (Neg y)
toNNF (Neg (x :\/ y)) = toNNF (Neg x) :/\ toNNF (Neg y)
toNNF (Neg (x :-> y)) = toNNF x       :/\ toNNF (Neg y)
