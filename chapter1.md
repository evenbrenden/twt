# Chapter 1

> **Exercise 1.2-i**
>
> Determine the cardinality of Either Bool (Bool,
Maybe Bool) -> Bool.

```
|Bool| ^ (|Bool|*|(Bool, |Maybe Bool|)|) =
2^(2 + 2*(1 + 2)) =
2^8 =
256
```

> **Exercise 1.4-i**
>
> Use Curry–Howard to prove that `(a^b)^c = a^(bc)`. That is, provide a function of type `(b -> c -> a) -> (b, c) -> a`, and one of `((b, c) -> a) -> b -> c -> a`. Make sure they satisfy the equalities `to . from = id` and `from . to = id`. Do these functions remind you of anything from `Prelude`?

```
to :: (b -> c -> a) -> (b, c) -> a
to bca (b, c) = bca b c

from :: ((b, c) -> a) -> b -> c -> a
from bca b c = bca (b, c)
```

> **Exercise 1.4-ii**
>
> Give a proof of the exponent law that `a^b*a^c = a^(b + c)`.

```
-- to :: (b -> a, c -> a) -> (Either b c -> a)
to :: (b -> a, c -> a) -> Either b c -> a
to (ba, ca) (Left b) = ba b
to (ba, ca) (Right c) = ca c

from :: (Either b c -> a) -> (b -> a, c -> a)
from ebca = (ebca . Left, ebca . Right)
```

> **Exercise 1.4-iii**
>
> Prove `(a*b)^c = a^c*b^c`.

```
to :: (c -> (a, b)) -> (c -> a, c -> b)
to cab = (fst . cab, snd . cab)

-- from :: (c -> a, c -> b) -> (c -> (a, b))
from :: (c -> a, c -> b) -> c -> (a, b)
from (ca, cb) c = (ca c, cb c)
```
