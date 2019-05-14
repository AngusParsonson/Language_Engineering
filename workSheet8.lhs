> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE InstanceSigs #-}

> import Text.Yoda
> import Data.Char
> import Data.Bool
> import Data.Map

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

> instance Alg Add Int where alg (Add x y) = x + y
> instance Alg Mul Int where alg (Mul x y) = x ∗ y
> instance Alg Sub Int where alg (Sub x y) = x − y
