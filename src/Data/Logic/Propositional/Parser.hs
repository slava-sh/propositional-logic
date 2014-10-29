module Data.Logic.Propositional.Parser
  ( parseExpr
  ) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Data.Functor.Identity
import Control.Applicative hiding ((<|>))

import Data.Logic.Propositional

parseExpr :: SourceName -> String -> Either ParseError Expr
parseExpr = runP expr ()

expr :: Parser Expr
expr = spaces *> buildExpressionParser table term
  <?> "expression"

term :: Parser Expr
term = parens expr <|> var
  <?> "term"

var :: Parser Expr
var = Var <$> (letter <* spaces)
  <?> "variable"

table :: OperatorTable String u Identity Expr
table =
  [ [unary  "~"  Neg]
  , [binary "&"  (:/\)]
  , [binary "|"  (:\/)]
  , [binary "->" (:->)]
  ]
  where
    unary   op f = Prefix $ parseOp op f `chainl1` return (.)
    binary  op f = Infix (parseOp op f) AssocRight
    parseOp op f = string op *> spaces *> return f

parens :: Parser a -> Parser a
parens = between (char '(' *> spaces) (char ')' *> spaces)
