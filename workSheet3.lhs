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

> data ValF k = ValF Int
> data AddF k = AddF k k
> data SubF k = SubF k k

> instance Functor SubF where
>   fmap f (SubU x y) = SubF (f x)(f y)

> instance Functor ValF where
>   fmap f (ValU x) = ValF x

> instance Functor AddF where
>   fmap f (AddU x y) = AddF (f x)(f y)

> evalAddSub :: Fix (ValF :+: AddF :+: SubF) -> Int
> evalAddSub = cata alg
>   where alg (L (ValF x))       = x
>         alg (R (L (AddF x y))) = x + y
>         alg (R (R (SubF x y))) = x - y

> class Functor f => Alg f a where
>   alg :: f a -> a

> instance Alg ValF Int where
>   alg (ValU x) = x

> instance Alg AddF Int where
>   alg (AddU x y) = x + y

> instance Alg SubF Int where
>   alg (SubU x y) = x - y

> instance Alg MulF Int where
>   alg (MulU x y) = x * y

> instance Functor MulF where
>   fmap f (MulU x y) = MulU (f x)(f y)

> type ExprF = Fix (ValF :+: AddF :+: SubF :+: MulF)

> evalF :: Expr -> Int
> evalF = cati

> instance (Alg f a, Alg g a) => Alg (f :+: g) a where
> alg (L x) = alg x
> alg (R y) = alg y

> evalAddSubF :: Fix (ValF :+: AddF :+: SubF) -> Int
> evalAddSubF = cata alg

> cati :: Alg f a => Fix f -> a
> cati = alg . fmap cati . inop
