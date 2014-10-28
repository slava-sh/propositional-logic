import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Control.Applicative
import Data.Maybe

import Data.Logic.Propositional

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcTests, unitTests]

var  = Variable
neg  = Negation
conj = Conjunction
disj = Disjunction
impl = Implication

p = var 'p'
q = var 'q'
r = var 'r'

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

eval :: [(Atom, Bool)] -> Expr -> Bool
eval env = eval'
    where
        eval' (Variable x)      = fromJust $ lookup x env
        eval' (Negation x)      = not $ eval' x
        eval' (Conjunction x y) = eval' x && eval' y
        eval' (Disjunction x y) = eval' x || eval' y
        eval' (Implication x y) = not (eval' x) || eval' y

isCNF :: Expr -> Bool
isCNF (Conjunction x y) = isCNF x && isCNF y
isCNF x                 = isDisj x
    where
        isDisj (Disjunction x y)      = isDisj x && isDisj y
        isDisj x                      = isLit x

        isLit (Variable _)            = True
        isLit (Negation (Variable _)) = True
        isLit _                       = False

qcTests = testGroup "QuickCheck tests"
    [ testProperty "CNF of x is semantically equivalent to x" $ \x ->
          truthTable (toCNF x) == truthTable x
    , testProperty "CNF is a conjunction of disjuncitons" $ \x ->
          isCNF $ toCNF x
    ]

cnfs =
    [ p
    , p `disj` neg q
    , (neg p `disj` (r `disj` neg q)) `disj` (neg q `disj` (neg p `disj` q))
    , p `conj` q `conj` neg r `conj` r
    ]

notCnfs =
    [ (neg p `disj` neg (r `disj` neg q)) `disj` p
    --              ^       ^
    , (neg p `disj` (r `conj` neg q)) `disj` p
    --                  ^
    , (neg p `disj` (r `impl` neg q)) `disj` p
    --                  ^
    , (p `disj` neg (neg q))
    --          ^     ^
    ]

unitTests = testGroup "Unit tests"
    [ testCase "CNF of an atom: p" $ toCNF p @?= p
    , testCase "CNF of an atom: q" $ toCNF q @?= q
    , testCase "CNF of an atom: r" $ toCNF r @?= r
    , testCase "isCNF: True"  $ True  @=? all isCNF cnfs
    , testCase "isCNF: False" $ False @=? any isCNF notCnfs
    ]
