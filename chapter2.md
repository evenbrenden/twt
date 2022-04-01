# chapter2

> If `Show Int` has kind `CONSTRAINT`, what's the kind of `Show`?

```
Show :: Type -> Constraint
```
> What is the knd of `Functor`?

```
Functor :: (Type -> Type) -> Constraint
```

> What is the kind of `Monad`?

```
Monad :: (Type -> Type) -> Constraint
```

> What is the kind of `MonadTrans`?

```
MonadTrans :: ((Type -> Type) -> Type -> Type) -> Constraint
```

> Write a closed type family to compute `Not`.

```
type family Not (x :: Bool) :: Bool where
    Not 'True = 'False
    Not 'False = 'True
```
