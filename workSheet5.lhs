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
