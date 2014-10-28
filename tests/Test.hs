import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Control.Applicative
import Data.Maybe

import Data.Logic.Propositional

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcTests, unitTests]

lit     = Literal
not'    = Negation
and'    = Conjunction
or'     = Disjunction
implies = Implication

p = lit 'p'
q = lit 'q'
r = lit 'r'

instance Arbitrary Expr where
    arbitrary = sized expr
        where
            expr 0 = elements [p, q, r]
            expr n = oneof
                [ Negation <$> expr n'
                , Conjunction <$> expr n' <*> expr n'
                , Disjunction <$> expr n' <*> expr n'
                , Implication <$> expr n' <*> expr n'
                ]
                where
                    n' = n `div` 2

truthTable :: Expr -> [Bool]
truthTable x = do
    p <- [False, True]
    q <- [False, True]
    r <- [False, True]
    return $ eval [('p', p), ('q', q), ('r', r)] x

eval :: [(Variable, Bool)] -> Expr -> Bool
eval env = eval'
    where
        eval' (Literal x)       = fromJust $ lookup x env
        eval' (Negation x)      = not $ eval' x
        eval' (Conjunction x y) = eval' x && eval' y
        eval' (Disjunction x y) = eval' x ||  eval' y
        eval' (Implication x y) = not (eval' x) || eval' y

qcTests = testGroup "QuickCheck tests"
    [ QC.testProperty "CNF of x is semantically equivalent to x" $ \x ->
          truthTable (conjunctiveNormalForm x) == truthTable x
    ]

unitTests = testGroup "Unit tests"
    [ testCase "CNF of an atom: p" $
        conjunctiveNormalForm p @?= p
    , testCase "CNF of an atom: q" $
        conjunctiveNormalForm q @?= q
    , testCase "CNF of an atom: r" $
        conjunctiveNormalForm r @?= r
    ]
