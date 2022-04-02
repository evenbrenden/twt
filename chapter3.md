# Chapter 3

> **Exercise 3-i**
>
> Which of these types are `Functor`s? Give instances for the ones that are.

```
newtype T1 a = T1 (Int -> a)

instance Functor T1 where
    fmap f (T1 ia) = T1 $ \i -> f (ia i)

newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T5 where
    fmap f (T5 aii) = T5 $ \ai -> aii $ ai . f
```
