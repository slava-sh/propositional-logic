import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Control.Applicative
import Data.Maybe
import Data.Function (on)

import Data.Logic.Propositional
import Data.Logic.Propositional.Parser

main = defaultMain tests

tests :: TestTree
tests = testGroup "Data.Logic.Propositional.Parser"
  [ testParseExpr "p" p
  , testParseExpr "q" q
  , testParseExpr "     q" q
  , testParseExpr "q     " q
  , testParseExpr " ( (q ) ) " q
  , testParseExpr "(  (  ( ((q)   )  ) ))" q
  , testParseExpr "~  ~  ( ~(q)   )" $ Neg (Neg ( Neg q))
  , testParseExpr "(~p | ~(q | ~q)) | p" $ (Neg p :\/ Neg (q :\/ Neg q)) :\/ p
  , testParseExpr "(~p | r & ~q) | p" $ (Neg p :\/ (r :/\ Neg q)) :\/ p
  , testParseExpr "(~p | r -> ~q) | p" $ ((Neg p :\/ r) :-> Neg q) :\/ p
  , testParseExpr "p | ~~q" $ p :\/ Neg (Neg q)
  , testParseExpr "(~p & (r | ~q)) & p" $ (Neg p :/\ (r :\/ Neg q)) :/\ p
  , testParseExpr "(~p | (r -> q)) | p" $ (Neg p :\/ (r :-> q)) :\/ p
  , testParseExpr "p | ~~q" $ p :\/ Neg (Neg q)
  , testParseExpr "p -> q -> r -> p & q & r | p & q | r -> q" $
      p :-> (q :-> (r :-> (((p :/\ (q :/\ r)) :\/ ((p :/\ q) :\/ r)) :-> q)))
  ]

testParseExpr :: String -> Expr -> TestTree
testParseExpr s e = testCase ("parseExpr: " ++ s) $
  case parseExpr "" s of
    Left err -> assertFailure $ show err
    Right x  -> x @?= e

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
