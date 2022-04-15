# Exercise 12-iii

> See what happens when you directly add a `TypeError` to the context of a function (eg. `foo :: TypeError ... => a`). What happens? Do you know why?

```
> :set -XDataKinds
> import GHC.TypeLits
> foo :: TypeError ('Text "Error!") => a -> a; foo = id

<interactive>:3:8: error:
    • Error!
    • In the type signature:
        foo :: TypeError ('Text "Error!") => a -> a
```

In this case, the error will always be thrown when the constraints are resolved. This is why you need to use a type family to calculate whether or not the error should be thrown.
