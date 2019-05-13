> data Expr = Val Int
>           | Add Expr Expr
>   deriving Show

> eval :: Expr -> Int
> eval (Val x)     = x
> eval (Add el er) = eval(el) + eval(er)

> data Fix f = In (f (Fix f))

> inop :: Fix f -> f (Fix f )
> inop (In x) = x

> data ExprU k = ValU Int
>              | AddU k k

> type Expr' = Fix (ExprU)

Let Expr′ be a type alias for Fix ExprU . Write down three values of type Expr′
- In ( ValU 3 )
- In ( AddU ( In ( ValU 4 ) ) ( In ( ValU 6 )))
- In ( AddU ( In ( AddU ( In ( ValU 4)) (In (ValU 3)))) (In ( ValU 7 )))

> fromExpr :: Expr -> Expr'
> fromExpr (Val x)   = In ( ValU x )
> fromExpr (Add x y) = In ( AddU (fromExpr x)(fromExpr y))

> cata :: Functor f => (f a -> a) -> Fix f -> a
> cata alg x = (alg . fmap (cata alg) . inop) x

> instance Functor ExprU where
>   fmap f (ValU x)   = (ValU x)
>   fmap f (AddU x y) = AddU (f x)(f y)

> eval' :: Expr' -> Int
> eval' = cata alg
>   where
>     alg (ValU x)   = x
>     alg (AddU x y) = x + y

> toExpr :: Expr' -> Expr
> toExpr = cata alg
>   where
>     alg (ValU x)    = (Val x)
>     alg (AddU x y)  = (Add x y)

> data (f :+: g) a = L (f a) | R (g a)
> infixr 5 :+:

> instance (Functor f, Functor g) => Functor (f :+: g) where
>   fmap f (L x) = L (fmap f x)
>   fmap f (R x) = R (fmap f x)

> data ValU k = ValU Int
> data AddU k = AddU k k
> data SubU k = SubU k k

> instance Functor SubU where
>   fmap f (SubU x y) = SubU (f x)(f y)

> instance Functor ValU where
>   fmap f (ValU x) = ValU x

> instance Functor AddU where
>   fmap f (AddU x y) = AddU (f x)(f y)

> evalAddSub :: Fix (ValU :+: AddU :+: SubU) -> Int
> evalAddSub = cata alg
>   where alg (L (ValU x))       = x
>         alg (R (L (AddU x y))) = x + y
>         alg (R (R (SubU x y))) = x - y

> class Functor f => Alg f a where
>   alg :: f a -> a

> instance Alg ValU Int where
>   alg (ValU x) = x

> instance Alg AddU Int where
>   alg (AddU x y) = x + y

> instance Alg SubU Int where
>   alg (SubU x y) = x - y

> instance Alg MulU Int where
>   alg (MulU x y) = x * y

> instance Functor MulU where
>   fmap f (MulU x y) = MulU (f x)(f y)

> type Expr = Fix (Val :+: Add :+: Sub :+: Mul)

> eval :: Expr -> Int
> eval = cati

> instance (Alg f a, Alg g a) => Alg (f :+: g) a where
> alg (L x) = alg x
> alg (R y) = alg y

> evalAddSub :: Fix (Val : + : Add : + : Sub) -> Int
> evalAddSub = cata alg

> cati :: Alg f a => Fix f -> a
> cati = alf . fmap cati . inop
