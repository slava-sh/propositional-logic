import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Control.Applicative
import Data.Maybe
import Data.Function (on)

import Data.Logic.Propositional

main = defaultMain tests

tests :: TestTree
tests = testGroup "Data.Logic.Propositional"
  [ testCase "CNF of an atom: p" $ toCNF p @?= p
  , testCase "CNF of an atom: q" $ toCNF q @?= q
  , testCase "CNF of an atom: r" $ toCNF r @?= r
  , testCase "DNF of an atom: p" $ toDNF p @?= p
  , testCase "DNF of an atom: q" $ toDNF q @?= q
  , testCase "DNF of an atom: r" $ toDNF r @?= r
  , testCase "isCNF: True"  $ True  @=? all isCNF cnfs
  , testCase "isCNF: False" $ False @=? any isCNF notCnfs
  , testCase "isDNF: True"  $ True  @=? all isDNF dnfs
  , testCase "isDNF: False" $ False @=? any isDNF notDnfs
  , testCase "show Expr" $
      show (p :\/ q :\/ r :/\ r :\/ ((r :\/ q) :/\ q) :\/ q)
        @?= "(p ∨ (q ∨ ((r ∧ r) ∨ (((r ∨ q) ∧ q) ∨ q))))"
  , testCase "show Expr" $
      show (p :/\ q :-> p :\/ q :-> Neg r)
        @?= "((p ∧ q) → ((p ∨ q) → ¬r))"
  , testProperty "CNF of x is semantically equivalent to x" $ \x ->
      toCNF x `equals` x
  , testProperty "DNF of x is semantically equivalent to x" $ \x ->
      toDNF x `equals` x
  , testProperty "NNF of x is semantically equivalent to x" $ \x ->
      toNNF x `equals` x
  , testProperty "CNF is a conjunction of disjuncitons of literals" $ \x ->
      isCNF $ toCNF x
  , testProperty "DNF is a disjunction of conjuncitons of literals" $ \x ->
      isDNF $ toDNF x
  , testProperty "In NNF only atoms are negated" $ \x ->
      not . containsNonatomicNegations $ toNNF x
  ]

cnfs =
  [ p
  , p :\/ Neg q
  , (Neg p :\/ (r :\/ Neg q)) :\/ (Neg q :\/ (Neg p :\/ q))
  , p :/\ q :/\ Neg r :/\ r
  , p :/\ q :/\ (Neg r :\/ ((q :\/ r) :\/ p)) :/\ r
  ]

notCnfs =
  [ (Neg p :\/ Neg (r :\/ Neg q)) :\/ p
  --           ^      ^
  , (Neg p :\/ (r :/\ Neg q)) :\/ p
  --              ^
  , (Neg p :\/ (r :-> Neg q)) :\/ p
  --              ^
  , p :\/ Neg (Neg q)
  --      ^    ^
  ]

dnfs =
  [ p
  , p :\/ Neg q
  , (Neg p :\/ (r :\/ Neg q)) :\/ (Neg q :\/ (Neg p :\/ q))
  , p :/\ q :/\ Neg r :/\ r
  , p :\/ q :\/ (Neg r :/\ ((q :/\ r) :/\ p)) :\/ r
  ]

notDnfs =
  [ (Neg p :\/ Neg (r :\/ Neg q)) :\/ p
  --           ^      ^
  , (Neg p :/\ (r :\/ Neg q)) :/\ p
  --              ^
  , (Neg p :\/ (r :-> Neg q)) :\/ p
  --              ^
  , p :\/ Neg (Neg q)
  --      ^    ^
  ]

p = Var 'p'
q = Var 'q'
r = Var 'r'

instance Arbitrary Expr where
  arbitrary = sized $ \n -> resize (n `div` 2) $ do
    frequency
      [ (1, elements [p, q, r])
      , (n, (Neg) <$> arbitrary)
      , (n, (:/\) <$> arbitrary <*> arbitrary)
      , (n, (:\/) <$> arbitrary <*> arbitrary)
      , (n, (:->) <$> arbitrary <*> arbitrary)
      ]

truthTable :: Expr -> [Bool]
truthTable x = do
  p <- [False, True]
  q <- [False, True]
  r <- [False, True]
  return $ eval [('p', p), ('q', q), ('r', r)] x

eval :: [(Atom, Bool)] -> Expr -> Bool
eval env = go
  where
    go (Var x)   = fromJust $ lookup x env
    go (Neg x)   = not $ go x
    go (x :/\ y) = go x && go y
    go (x :\/ y) = go x || go y
    go (x :-> y) = not (go x) || go y

equals :: Expr -> Expr -> Bool
equals = (==) `on` truthTable

isLit :: Expr -> Bool
isLit (Var _)       = True
isLit (Neg (Var _)) = True
isLit _             = False

isCNF :: Expr -> Bool
isCNF (x :/\ y) = isCNF x && isCNF y
isCNF x         = isDisj x
  where
    isDisj (x :\/ y) = isDisj x && isDisj y
    isDisj x         = isLit x

isDNF :: Expr -> Bool
isDNF (x :\/ y) = isDNF x && isDNF y
isDNF x         = isConj x
  where
    isConj (x :/\ y) = isConj x && isConj y
    isConj x         = isLit x

containsNonatomicNegations :: Expr -> Bool
containsNonatomicNegations = go
  where
    go (Var _)       = False
    go (Neg (Var _)) = False
    go (Neg _)       = True
    go (x :/\ y)     = go x || go y
    go (x :\/ y)     = go x || go y
    go (x :-> y)     = go x || go y
