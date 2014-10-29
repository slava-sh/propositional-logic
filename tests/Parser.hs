import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Control.Applicative
import Data.Maybe
import Data.Function (on)

import Data.Logic.Propositional
import Data.Logic.Propositional.Parser

import Utils

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
  , testProperty "parseExpr . show == id" $ \expr ->
      case parseExpr "" (show expr) of
        Left _  -> False
        Right x -> x == expr
  ]

testParseExpr :: String -> Expr -> TestTree
testParseExpr s e = testCase ("parseExpr: " ++ s) $
  case parseExpr "" s of
    Left err -> assertFailure $ show err
    Right x  -> x @?= e
