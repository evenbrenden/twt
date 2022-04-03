# Chapter 7

> **Exercise 7.1-i**
>
> Are functions of type `forall a. a -> r` interesting? Why or why not?

Since we know nothing of `a`, we cannot do anything with it. Therefore all we can do is return an `r`, which is not very interesting.

> **Exercise 7.1-ii**
>
> What happens to this instance if you remove the `Show t =>` constraint from `HasShow`?

Since the `t` in `HasShow` is existential, there is no way to decide on a `Show` instance for `t` at the call site. Therefore, the compiler will complain about a missing `Show` instance for `s` at the call site `show` of `instance Show HasShow`.
