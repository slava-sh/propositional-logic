import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Control.Applicative
import Data.Maybe

import Data.Logic.Propositional

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcTests, unitTests]

p = Var 'p'
q = Var 'q'
r = Var 'r'

instance Arbitrary Expr where
    arbitrary = sized expr
        where
            expr 0 = elements [p, q, r]
            expr n = oneof
                [ (Neg) <$> expr n'
                , (:/\) <$> expr n' <*> expr n'
                , (:\/) <$> expr n' <*> expr n'
                , (:->) <$> expr n' <*> expr n'
                ]
                where
                    n' = n `div` 2

truthTable :: Expr -> [Bool]
truthTable x = do
    p <- [False, True]
    q <- [False, True]
    r <- [False, True]
    return $ eval [('p', p), ('q', q), ('r', r)] x

eval :: [(Atom, Bool)] -> Expr -> Bool
eval env = eval'
    where
        eval' (Var x)   = fromJust $ lookup x env
        eval' (Neg x)   = not $ eval' x
        eval' (x :/\ y) = eval' x && eval' y
        eval' (x :\/ y) = eval' x || eval' y
        eval' (x :-> y) = not (eval' x) || eval' y

isCNF :: Expr -> Bool
isCNF (:/\ x y) = isCNF x && isCNF y
isCNF x         = isDisj x
    where
        isDisj (:\/ x y) = isDisj x && isDisj y
        isDisj x         = isLit x

        isLit (Var _)       = True
        isLit (Neg (Var _)) = True
        isLit _             = False

containsNonatomicNegs :: Expr -> Bool
containsNonatomicNegs = go
    where
        go (Var _)       = False
        go (Neg (Var _)) = False
        go (Neg _)       = True
        go (x :/\ y)     = go x || go y
        go (x :\/ y)     = go x || go y
        go (x :-> y)     = go x || go y

qcTests = testGroup "QuickCheck tests"
    [ testProperty "CNF of x is semantically equivalent to x" $ \x ->
          truthTable (toCNF x) == truthTable x
    , testProperty "CNF is a :/\ of (:\/)uncitons" $ \x ->
          isCNF $ toCNF x
    , testProperty "NNF of x is semantically equivalent to x" $ \x ->
          truthTable (toNNF x) == truthTable x
    , testProperty "In NNF only atoms are Negated" $ \x ->
          not . containsNonatomicNegs $ toNNF x
    ]

cnfs =
    [ p
    , p :\/ Neg q
    , (Neg p :\/ (r :\/ Neg q)) :\/ (Neg q :\/ (Neg p :\/ q))
    , p :/\ q :/\ Neg r :/\ r
    ]

notCnfs =
    [ (Neg p :\/ Neg (r :\/ Neg q)) :\/ p
    --           ^      ^
    , (Neg p :\/ (r :/\ Neg q)) :\/ p
    --              ^
    , (Neg p :\/ (r :-> Neg q)) :\/ p
    --              ^
    , (p :\/ Neg (Neg q))
    --       ^    ^
    ]

unitTests = testGroup "Unit tests"
    [ testCase "CNF of an atom: p" $ toCNF p @?= p
    , testCase "CNF of an atom: q" $ toCNF q @?= q
    , testCase "CNF of an atom: r" $ toCNF r @?= r
    , testCase "isCNF: True"  $ True  @=? all isCNF cnfs
    , testCase "isCNF: False" $ False @=? any isCNF notCnfs
    ]
