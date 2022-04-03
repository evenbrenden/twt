# Chapter 6.3

> **Exercise 6.3-i**
>
> What is the rank of `Int -> forall a. a -> a`? Hint: try adding the explicit parentheses.

```
Int -> forall a. a -> a ~
Int -> forall a. (a -> a)
```

The deepest `forall` has 1 arrow to the left of it, so it is rank-1.

> **Exercise 6.3-ii**
>
> What is the rank of `(a -> b) -> (forall c. c -> a) -> b`? Hint: recall that the function arrow is right-associative, so `a -> b -> c` is actually parsed as `a -> (b -> c)`.

```
(a -> b) -> (forall c. c -> a) -> b ~
forall a b. ((a -> b) -> ((forall c. c -> a) -> b))
```

The deepest `forall` has 2 arrows to the left of it, so it is rank-2.

> **Exercise 6.3-iii**
>
> What is the rank of `((forall x. m x -> b (z m x)) -> b (z m a)) -> m a`? Believe it or not, this is a real type signature we had to write back in the bad old days before `MonadUnliftIO`!

```
((forall x. m x -> b (z m x)) -> b (z m a)) -> m a ~
forall m b z a. (((forall x. m x -> b (z m x)) -> b (z m a)) -> m a)
```

The deepest `forall` has 3 arrows to the left of it, so it is rank-3.
