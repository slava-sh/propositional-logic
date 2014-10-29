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
  [ map (unary  (Neg)) ["¬", "~"]
  , map (binary (:/\)) ["∧", "&"]
  , map (binary (:\/)) ["∨", "|"]
  , map (binary (:->)) ["→", "->"]
  ]
  where
    unary    f op = Prefix $ operator f op `chainl1` return (.)
    binary   f op = Infix (operator f op) AssocRight
    operator f op = string op *> spaces *> return f

parens :: Parser a -> Parser a
parens = between (char '(' *> spaces) (char ')' *> spaces)
