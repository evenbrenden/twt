# Chapter 2

> **Exercise 2.1.3-i**
>
> If `Show Int` has kind `CONSTRAINT`, what's the kind of `Show`?

```
Show :: Type -> Constraint
```

> **Exercise 2.1.3-ii**
>
> What is the knd of `Functor`?

```
Functor :: (Type -> Type) -> Constraint
```

> **Exercise 2.1.3-iii**
>
> What is the kind of `Monad`?

```
Monad :: (Type -> Type) -> Constraint
```

> **Exercise 2.1.3-iv**
>
> What is the kind of `MonadTrans`?

```
MonadTrans :: ((Type -> Type) -> Type -> Type) -> Constraint
```

> **Exercise 2.1.4-i**
>
> Write a closed type family to compute `Not`.

```
type family Not (x :: Bool) :: Bool where
    Not 'True = 'False
    Not 'False = 'True
```
