> module Lecture12 where

> import Text.Yoda
> import Data.Char

Let's start with a left-recursive grammar:

    expr = term | expr "+" term
    term = fact | term "*" fact
    fact = numb | "(" expr ")"
    numb = "0" | "1" | "2"

We must remove the left recursion by applying Paull's modified
algorithm:

    expr  = term expr'
    expr' = "+" term expr'
          | epsilon

    term  = fact term'
    term' = "*" fact term' | epsilon

    fact = numb | "(" expr ")"
    numb = "0" | "1" | "2"

The other rules remain the same.

We now make datatypes that correspond to these rules.
Each nonterminal corresponds to a new datatype. The alternations
correspond to constructors for that type.

> data Expr  = ExprTerm Term Expr'
>   deriving Show

> expr :: Parser Expr
> expr = ExprTerm <$> term <*> expr'

    expr' = "+" term expr'
          | epsilon

> data Expr' = Expr'Add Term Expr'
>            | Expr'Emp
>   deriving Show

> expr' :: Parser Expr'
> expr' = Expr'Add <$ char '+' <*> term <*> expr'
>     <|> Expr'Emp <$ unit

    term  = fact term'

> data Term  = TermFact Fact Term'
>   deriving Show

> term :: Parser Term
> term = TermFact <$> fact <*> term'

    term' = "*" fact term'
          | epsilon

> data Term' = Term'Mul Fact Term'
>            | Term'Emp
>   deriving Show

> term' :: Parser Term'
> term' = Term'Mul <$ char '*' <*> fact <*> term'
>     <|> Term'Emp <$ unit

    fact = numb
         | "(" expr ")"

> data Fact = FactNumb Int
>           | FactPar Expr
>   deriving Show

> fact = FactNumb <$> numb
>    <|> FactPar <$ char '(' <*> expr <* char ')'

    numb = "0" | "1" | "2"

> numb :: Parser Int
> numb  = read <$> digits

> digit :: Parser Char
> digit = satisfy isDigit

> digits :: Parser String
> digits = cull (some digit)

Now we can test this with statements such as:

    parse (expr) "(3+2)*45"

It is possible to remove spurious results by applying `eof` at the
end:

   parse (expr <* eof) "(3+2)*45"
