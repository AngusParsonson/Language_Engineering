> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE DeriveFunctor #-}

> import Text.Yoda
> import Data.Char
> import Data.Bool
> import Data.Map

> data Free f a = Var a | Op (f (Free f a))

> class Functor f => Alg f a where
>   alg :: f a -> a

> data (f :+: g) a = L (f a) | R (g a)
> infixr 5 :+:

> instance (Functor f, Functor g) => Functor (f :+: g) where
>   fmap f (L x) = L (fmap f x)
>   fmap f (R x) = R (fmap f x)

> type Expr = Add :+: Mul :+: Sub

> data Add k = Add k k deriving (Functor , Show)
> data Mul k = Mul k k deriving (Functor , Show)
> data Sub k = Sub k k deriving (Functor , Show)
> data Div k = Div k k deriving (Functor , Show)
> data Fail k = Fail deriving Functor

> instance Alg Add Int where alg (Add x y) = x + y
> instance Alg Mul Int where alg (Mul x y) = x * y
> instance Alg Sub Int where alg (Sub x y) = x - y
> instance Alg Div Int where alg (Div x y) = div x y

> instance Alg Fail (Maybe a) where
>   alg Fail = Nothing

> instance Alg Div (Maybe Int) where
>   alg (Div _ Nothing)  = Nothing
>   alg (Div Nothing _)  = Nothing
>   alg (Div _ (Just 0)) = Nothing
>   alg (Div (Just x) (Just y)) = Just (div x y)

Using eval and an appropriate generator, write a function
divEval :: Free (Expr :+: Div) Int → Maybe Int
which evaluates arithmetic expressions with division such that the variables
contain integers. You will need to define appropriate algebras for the Add,
Sub and Mul datatypes.

> eval :: Alg f b => (a -> b) -> Free f a -> b
> eval gen (Var x ) = gen x
> eval gen (Op op) = alg (fmap (eval gen) op)

> divEval :: Free (Expr :+: Div) Int -> Maybe Int
> divEval = eval Just
