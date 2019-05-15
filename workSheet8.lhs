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

> instance Functor f => Functor (Free f) where
>   fmap f (Var x) = Var (f x)
>   fmap f (Op x)  = Op (fmap(fmap f) x)

> data (f :+: g) a = L (f a) | R (g a)
> infixr 5 :+:

> instance (Functor f, Functor g) => Functor (f :+: g) where
>   fmap f (L x) = L (fmap f x)
>   fmap f (R x) = R (fmap f x)

> instance (Alg f a, Alg g a) => Alg (f :+: g) a where
>   alg (L x) = alg x
>   alg (R x) = alg x

> type Expr = Add :+: Mul :+: Sub

> data Or k = Or k k deriving Functor

> data Add k = Add k k deriving (Functor , Show)
> data Mul k = Mul k k deriving (Functor , Show)
> data Sub k = Sub k k deriving (Functor , Show)
> data Div k = Div k k deriving (Functor , Show)
> data Fail k = Fail deriving Functor

> instance Alg Or [a] where
>   alg (Or x y) = x ++ y

> instance Alg Add Int where alg (Add x y) = x + y
> instance Alg Mul Int where alg (Mul x y) = x * y
> instance Alg Sub Int where alg (Sub x y) = x - y
> instance Alg Div Int where alg (Div x y) = div x y

> instance Alg Add (Maybe Int) where
>   alg (Add (Just x) (Just y)) = Just (x + y)
>   alg _       = Nothing

> instance Alg Mul (Maybe Int) where
>   alg (Mul (Just x) (Just y)) = Just (x * y)
>   alg _       = Nothing

> instance Alg Sub (Maybe Int) where
>   alg (Sub (Just x) (Just y)) = Just (x - y)
>   alg _       = Nothing

> instance Alg Fail (Maybe a) where
>   alg Fail = Nothing

> instance Alg Div (Maybe Int) where
>   alg (Div _ (Just 0)) = Nothing
>   alg (Div (Just x) (Just y)) = Just (div x y)
>   alg _       = Nothing

 instance Alg Expr (Int) where
   alg (L (x)) = alg x
   alg (R (L (x))) = alg x
   alg (R (R (x))) = alg x

 instance Alg Expr (Maybe Int) where
   alg (L (x)) = alg x
   alg (R (L (x))) = alg x
   alg (R (R (x))) = alg x

 instance Alg (Expr :+: Div) (Maybe Int) where
   alg (L (x)) = alg x
   alg (R (x)) = alg x

Using eval and an appropriate generator, write a function
divEval :: Free (Expr :+: Div) Int → Maybe Int
which evaluates arithmetic expressions with division such that the variables
contain integers. You will need to define appropriate algebras for the Add,
Sub and Mul datatypes.

> eval :: Alg f b => (a -> b) -> Free f a -> b
> eval gen (Var x ) = gen x
> eval gen (Op op) = alg (fmap (eval gen) op)

> normalEval :: Free (Expr) Int -> Int
> normalEval = eval id

> divEval :: Free (Expr :+: Div) Int -> Maybe Int
> divEval = eval Just

> sqrtEval :: Free (Sqrt) Double -> Double
> sqrtEval = eval id

> data Sqrt k = Sqrt k deriving (Functor , Show)

> instance Alg Sqrt Double where
>   alg (Sqrt x) = sqrt(x)

> instance Alg Sqrt [Double] where
>   alg (Sqrt xs) = [rts | x <- xs, x >= 0, rts <- [sqrt(x), -sqrt(x)]]

Often, branching is paired with failure to form nondeterminism. Write an
appropriate algebra for Fail k such that is it compatible with Nondet k where:

> type Nondet = Fail :+: Or

> instance Alg Fail [a] where alg Fail = []

Write an appropriate instance for Alg Div [Double ].

> instance Alg Div [Double] where
>   alg (Div xs ys) = [ x / y | x <- xs, y <- ys, y /= 0]

By writing appropriate algebras for Add, Sub and Mul and using eval with an
appropriate gen, define a function
evalSqrt :: Free (Expr :+: Div :+: Sqrt) Double → [Double]
which evaluates arithmetic expressions with division and square roots, handling
failure and nondeterministic choices correctly.

> instance Alg Add [Double] where
>   alg (Add xs ys) = [x + y | x <- xs, y <- ys]

> instance Alg Mul [Double] where
>   alg (Mul xs ys) = [x * y | x <- xs, y <- ys]

> instance Alg Sub [Double] where
>   alg (Sub xs ys) = [x - y | x <- xs, y <- ys]

> evalSqrt :: Free (Expr :+: Div :+: Sqrt) Double -> [Double]
> evalSqrt = eval gen where
>   gen a = [a]

example instructions:
evalSqrt (Op (R (R (Sqrt (Var 75)))))
evalSqrt (Op (R (L (Div (Var 75)(Var 3)))))
evalSqrt (Op (L (L (Add (Var 75)(Var 3)))))
evalSqrt (Op (L (R (L (Mul (Var 75)(Var 3))))))
evalSqrt (Op (L (R (R (Sub (Var 75)(Var 3))))))
