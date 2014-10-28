data Formula
    = Literal String
    | Not     Formula
    | And     Formula Formula
    | Or      Formula Formula
    | Implies Formula Formula

instance Show Formula where
    show formula = case formula of
            (Literal x)   -> x
            (Not x)       -> "¬" ++ parens x
            (And x y)     -> showBinary "∧" x y
            (Or x y)      -> showBinary "∨" x y
            (Implies x y) -> showBinary "→" x y
        where
            showBinary op x y = parens x ++ " " ++ op ++ " " ++ parens y
            parens (Literal x) = x
            parens x           = "(" ++ show x ++ ")"

conjunctiveNormalForm :: Formula -> Formula
conjunctiveNormalForm = conjunctiveNormalForm' . negationNormalForm . implicationFree

conjunctiveNormalForm' :: Formula -> Formula
conjunctiveNormalForm' = id

negationNormalForm :: Formula -> Formula
negationNormalForm = id

implicationFree :: Formula -> Formula
implicationFree = id

main :: IO ()
main = do
    putStrLn "Transform a propositional logic formula into an equivalent formula in CNF"
    let x = Not (Not (Literal "a" `Or` Not (Literal "b"))) `And` (Literal "a" `Implies` Literal "b")
    print x
    print $ conjunctiveNormalForm x
