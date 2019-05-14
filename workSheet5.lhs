pred ::= "t" | "f" | pred "&&" pred | "!" pred

> import Text.Yoda
> import Data.Char
> import Data.Bool

> data Pred = T
>           | F
>           | Not Pred
>           | And Pred Pred
>   deriving Show

> evalPred :: Pred -> Bool
> evalPred (T) = True
> evalPred (F) = False
> evalPred (Not p)     | (evalPred p == True) = False
>                      | otherwise            = True
> evalPred (And p1 p2) | evalPred p1          = evalPred p2
>                      | otherwise            = False

> whitespace :: Parser ()
> whitespace = skip (many (oneOf [' ', '\t', '\n']))
> token :: String -> Parser String
> token t = string t <* whitespace

> predParser :: Parser Pred
> predParser =   T <$ token "t"
>            <|> F <$ token "f"
>            <|> Not <$ token "!" <*> predParser
>            <|> And <$> predParser <* token "&&" <*> predParser

pred ::= "t" pred' | "f" pred' | "!" pred pred'
pred' ::= "&&" pred pred' | epsilon

> data Pred2 = T2 Pred2'
>            | F2 Pred2'
>            | Not2 Pred2 Pred2'
>   deriving Show

> data Pred2' = And2 Pred2 Pred2'
>             | Epsilon
>   deriving Show

> pred2Parser :: Parser Pred2
> pred2Parser =   T2 <$ token "t" <*> pred2'Parser
>             <|> F2 <$ token "f" <*> pred2'Parser
>             <|> Not2 <$ token "!" <*> pred2Parser <*> pred2'Parser

> pred2'Parser :: Parser Pred2'
> pred2'Parser =   And2 <$ token "&&" <*> pred2Parser <*> pred2'Parser
>              <|> pure Epsilon

pred ::= "t" | "f" | pred "&&" pred | "!" pred

> predChainParser :: Parser Pred
> predChainParser = chainr1 term (And <$ token "&&")
>   where term = prefix (Not <$ token "!") atom
>         atom =   T <$ token "t"
>              <|> F <$ token "f"

graph ::= graph -> graph | graph + graph | "[" {number} "]"
graph ::= graph (-> | +) graph | "[" {number} "]"

> number :: Parser Int
> number = read <$> some(oneOf(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']))

> data Graph = Empty | Vertex Int | Overlay Graph Graph | Connect Graph Graph
>   deriving Show

> graphParser :: Parser Graph
> graphParser = chainl1 term (Overlay <$ token "+" <|> Connect <$ token "->")
>   where term = token "[" *> Vertex <$> number <|> pure Empty <* token "]"

graph ::= graph "+" graph | connects
connects ::= connects "->" connects | atom
atom ::= "[" {number} "]"

> newGraphParser :: Parser Graph
> newGraphParser = chainl1 term (Overlay <$ token "+")
>   where term = chainl1 atom (Connect <$ token "->")
>         atom = token "[" *> Vertex <$> number <|> pure Empty <* token "]"

parse expr1 "1 + 3 * 4"

(<$>) :: (a -> b) -> Parser a -> Parser b
f <$> pa = pure f <*> pa
