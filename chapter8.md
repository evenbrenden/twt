# Chapter 8

> **Exercise 8.2-i**
>
> What is the role signature of `Either a b`?

Since data constructors count as applying `(->)`, the role signature of `Either a b` is `type role Either representational representational`.

> **Exercise 8.2-ii**
>
> What is the role signature of `Proxy a`?

Since the `a` of `Proxy a` is a phantom type, it is not strengthened from `phantom`, and the role signature of `Proxy a` is `type role Proxy phantom`.
