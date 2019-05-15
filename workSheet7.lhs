> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE InstanceSigs #-}

> import Text.Yoda
> import Data.Char
> import Data.Bool
> import Data.Map

- Op (Add (Var "4") (Var "3"))
- Op (Add (Op (Add (Var "4") (Var "3"))) (Op (Add (Var "7") (Var "10"))))

> data Free f a = Var a | Op (f (Free f a))

> class Functor f => Alg f a where
>   alg :: f a -> a

> data Add k = Add k k
> instance Functor Add where
>   fmap f (Add x y) = Add (f x ) (f y)

> instance Alg Add Int where
>   alg :: Add Int -> Int
>   alg (Add x y) = x + y

> eval :: Alg f b => (a -> b) -> Free f a -> b
> eval gen (Var x ) = gen x
> eval gen (Op op) = alg (fmap (eval gen) op)

- x is of type a
- op is of type Free f a
- fmap is of type (a -> b) -> f a -> f b
- Similar to cata except fix is now free, the alg is now gen, and cata is eval.
Op is the new constructor for free f a rather than In.

- Provide gen that sets x = 4, y = 6 and any other variable is 0. Show the
derivation of evaluating the function eval gen on the expression x + (y + z).


> xIs4yIs6 :: Free Add String -> Int
> xIs4yIs6 = eval gen where
>   gen "x" = 4
>   gen "y" = 6
>   gen _   = 0

- addyBoi executes expressions containing addition and constant variables
defined by the provided function.

> addyBoi :: (String -> Int) -> Free Add String -> Int
> addyBoi gen = eval gen

> exGen :: String -> Int
> exGen "x" = 50
> exGen "y" = 22
> exGen _   = 0

- addyBoi2 works with the provided map providing the definitions of the
constants instead of a function.
- map :: (a -> b) -> [a] -> [b]

> addyBoi2 :: Map String Int -> Free Add String -> Int
> addyBoi2 f = eval (f !)

- vars collects all the bound variables in a given expression. Where a is String,
the variables in x + y + z are ["x", "y", "z"].

> instance Alg Add [a] where
>   alg (Add vs us) = vs ++ us

> vars :: Free Add a -> [a]
> vars = eval (\ v -> [v])

- Define a function extract :: Alg f a ⇒ Free f a → a that evaluates a syntax
tree when the type of variables coincides with the carrier of the algebra.

> extract :: Alg f a => Free f a -> a
> extract = eval gen where
>   gen x = x

type While = Stm
data Stm = Skip | Ident ::= Aexp | Stm :> Stm | If Bexp Stm Stm | While Bexp Stm deriving Show
data Aexp = Num Integer | Ident Ident | Aexp :+Aexp | Aexp :∗Aexp | Aexp :−Aexp deriving Show
data Bexp = T | F | Aexp := Aexp | Aexp :⩽ Aexp | Bexp :&&Bexp | Not Bexp deriving Show
type Ident = String

 type While k = Aexp :+: Bexp :+: Stm

 data Stm k = Skip | Ident ::= k | k :> k | If k k k | While k k deriving (Show, Functor)
 data Aexp  k = Num Integer | Ident Ident | k :+ k | k :* k | k :− k deriving (Show, Functor)
 data Bexp k = T | F | k := k | k :< k | k :&& k | Not k deriving (Show, Functor)
 type Ident = String

- Provide appropriate instances of Alg for Stm, Aexp, and Bexp where the carrier
is [Ident], and use eval to define a function vars :: Free While a → [Ident]
which returns all variables referenced in a While program. For example, the
variables in x := 3; y := 4; x := z are x, y, and z.

 instance Alg Aexp [Ident] where
   alg (Num x)   = []
   alg (Ident x) = [x]
   alg (x :+ y)  = x ++ y
   alg (x :* y)  = x ++ y
   alg (x :- y)  = x ++ y

 instance Alg Bexp [Ident] where
   alg (T) = []
   alg (F) = []
   alg (x := y)  = x ++ y
   alg (x :< y)  = x ++ y
   alg (x :&& y) = x ++ y
   alg (Not x)   = x

 instance Alg Stm [Ident] where
   alg (Skip)       = []
   alg (x := y)     = x:y
   alg (x :> y)     = x ++ y
   alg (If x y z)   = concat [x, y, z]
   alg (While x y)  = x ++ y

 vars :: Free While a → [Ident]
 vars = eval (const [])
