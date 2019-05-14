> import Text.Yoda
> import Data.Char
> import Data.Bool

> number :: Parser Int
> number = read <$> some(oneOf(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']))

> class Pretty a where
>   pretty :: a -> String

> data Robot = Mov Int Robot | Rt Robot | Lt Robot | Stop
>   deriving (Eq, Show)

> robot :: Parser RobotS
> robot = (Mov <$ char 'f' <* optional (char ' ') <*> number
>        <|> Rt <$ string "r"
>        <|> Lt <$ string "l") <* string ", " <*> robot
>        <|> Stop <$ string "stop."

> instance Pretty Robot where
>   pretty (Mov x r) = "f " ++ show x ++ ", " ++ pretty (r)
>   pretty (Rt r)    = "r, " ++ pretty (r)
>   pretty (Lt r)    = "l, " ++ pretty (r)
>   pretty (Stop)    = "stop."

check, given a parser and an input string, will first parse the string to get a value of type a then pretty print it and parse it again
ensuring that both parses return the same value of type a

> check :: (Eq a, Pretty a) => Parser a -> String -> Bool
> check pa ts = fst (head (parse (cull pa) ts)) == fst (head (parse (cull pa) (pretty(fst (head (parse (cull pa) ts))))))

while ::= stms
stms ::= stms ";" stms | stm
stm ::= "skip" | var ":=" aexp | "(" stms ")"
        | "if" bexp "then" stm "else" stm
        | "while" bexp "do" stm

aexp ::= aexp "+" aexp | aexp "-" aexp | aterm
aterm ::= aterm "*" aterm | atom
atom ::= num | var | "(" aexp ")"

bexp ::= bexp "&&" bexp | bterm
bterm ::= "!" bterm | btom

btom ::= aexp "<=" aexp | aexp "=" aexp | "true" | "false" | "(" bexp ")"

num ::= ('0'..'9')+
var ::= ('a'..'z')+

> type While = Stm

> data Stm = Skip | Ident ::= Aexp | Stm :> Stm | If Bexp Stm Stm | While Bexp Stm

> data Aexp = Num Integer | Ident Ident | Aexp :+Aexp | Aexp :∗Aexp | Aexp :−Aexp

> data Bexp = T | F | Aexp := Aexp | Aexp :⩽ Aexp | Bexp :&&Bexp | Not Bexp

> type Ident = String

Write a definition for (⟨$⟩) :: (a → b) → Parser a → Parser b using pure and (⟨∗⟩)

> (<$:>) :: (a -> b) -> Parser a -> Parser b
> f <$:> pa = pure f <*> pa

Write a definition for (∗⟩) :: Parser a → Parser b → Parser b in terms of (⟨$⟩) and (⟨∗⟩).

> (*:>) :: Parser a -> Parser b -> Parser b
> pa *:> pb = flip const <$> pa <*> pb

Write a definition for (⟨∗) :: Parser a → Parser b → Parser a in terms of pure and (⟨∗⟩).

> (<:*) :: Parser a -> Parser b -> Parser a
> pa <:* pb = pure const <*> p <*> q
