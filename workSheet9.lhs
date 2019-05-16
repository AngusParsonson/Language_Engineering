> data Stack a k = Pop (a -> k) | Push a k

> data Free f a = Var a | Op (f (Free f a))

> instance Functor f => Functor (Free f) where
>   fmap f (Var x) = Var (f x)
>   fmap f (Op x)  = Op (fmap(fmap f) x)

> instance Functor (Stack a) where
>   fmap :: (k → b) → Stack a k → Stack a b
>   fmap f (Pop g) = Pop (f · g)
>   fmap f (Push x k) = Push x (f k)

push :: a -> Free (Stack a) ()
pop :: Free (Stack a) a
peek :: Free (Stack a) a
poke :: a -> Free (Stack a) ()

> push :: a -> Free (Stack a) ()
> push x = Op (Push x (Var ()))

> pop :: Free (Stack a) a
> pop = Op (Pop Var)

> peek :: Free (Stack a) a
> peek = do x <- pop
>                push x
>                return x

> poke :: a -> Free (Stack a) ()
> poke x = do pop
>             push x

By desugaring the push and pop operations, or otherwise, give definitions for
peek and poke purely in terms of the constructors Op, Var , Pop and Push.
The definition of (>>=) will be required when desugaring.
(>>=) :: m a -> (  a -> m b) -> m b
Free (Stack a) -> (a -> Free (Stack a)) -> Free (Stack a)

> poke :: a -> Free (Stack a) ()
> poke x = do Op (Pop Var)
