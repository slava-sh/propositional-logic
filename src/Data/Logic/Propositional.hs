module Data.Logic.Propositional where

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
conjunctiveNormalForm = conjunctiveNormalForm' . negationNormalForm . implicationFree

conjunctiveNormalForm' :: Expr -> Expr
conjunctiveNormalForm' = id

negationNormalForm :: Expr -> Expr
negationNormalForm = id

implicationFree :: Expr -> Expr
implicationFree = id
