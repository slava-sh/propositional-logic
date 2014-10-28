import Test.Tasty
import Test.Tasty.QuickCheck
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

isCNF :: Expr -> Bool
isCNF (Conjunction x y) = isCNF x && isCNF y
isCNF x                 = isDisjunctionOfLiterals x

isDisjunctionOfLiterals :: Expr -> Bool
isDisjunctionOfLiterals (Literal _)            = True
isDisjunctionOfLiterals (Negation (Literal _)) = True
isDisjunctionOfLiterals (Disjunction x y)      =
    isDisjunctionOfLiterals x && isDisjunctionOfLiterals y
isDisjunctionOfLiterals _                      = False

qcTests = testGroup "QuickCheck tests"
    [ testProperty "CNF of x is semantically equivalent to x" $ \x ->
          truthTable (conjunctiveNormalForm x) == truthTable x
    , testProperty "CNF is a conjunction of disjuncitons" $ \x ->
          isCNF $ conjunctiveNormalForm x
    ]

cnfs =
    [ p
    , p `or'` not' q
    , (not' p `or'` (r `or'` not' q)) `or'` (not' q `or'` (not' p `or'` q))
    , p `and'` q `and'` not' r `and'` r
    ]

notCnfs =
    [ (not' p `or'` not' (r `or'` not' q)) `or'` p
    --              ^        ^
    , (not' p `or'` (r `and'` not' q)) `or'` p
    --                  ^
    , (not' p `or'` (r `implies` not' q)) `or'` p
    --                  ^
    , (p `or'` not' (not' q))
    --         ^     ^
    ]

unitTests = testGroup "Unit tests"
    [ testCase "CNF of an atom: p" $ conjunctiveNormalForm p @?= p
    , testCase "CNF of an atom: q" $ conjunctiveNormalForm q @?= q
    , testCase "CNF of an atom: r" $ conjunctiveNormalForm r @?= r
    , testCase "isCNF: True" $ True  @=? all isCNF cnfs
    , testCase "isCNF: False" $ False @=? any isCNF notCnfs
    ]
