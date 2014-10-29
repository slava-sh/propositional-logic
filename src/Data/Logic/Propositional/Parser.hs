module Data.Logic.Propositional.Parser
  ( parseExpr
  ) where

import Text.Parsec
import Text.Parsec.Expr
import Control.Applicative

import Data.Logic.Propositional

parseExpr :: SourceName -> String -> Either ParseError Expr
parseExpr = runP expr ()

expr :: Parsec String () Expr
expr = buildExpressionParser operators term

term = do
  x <- letter
  return $ Var x

operators = []
