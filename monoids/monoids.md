
## Monoids are everywhere - You just weren't aware of them.

They are among us: you use them during your everyday life, while waiting for
the bus, when programming, when going out for a walk in the park or when doing
some math. In this brief journey into category
theory we'll meet Monoids, powerful structures you have been using since you
were a child. You don't trust me? Fair enough, I'll show you that there
is nothing intimidating about them, and they are indeed quite a powerful
abstraction tool, relevant not only in math but also in programming.


### A clock
Let's first point our attention to something as simple as a clock. To simplify a bit
and build our intuition, let's imagine our clock has only the hour hand, and let's
assume our clock makes no distinction between the 24 and the 12h format. In other terms,
12:00 will be the same for us, no matter if it's noon or midnight.
Now let's think about what happen if we add 12 hours to a clock. Well, it's not hard to
see that we'll do a complete rotation on the clock and we'll get back to the original
value we had in the first place. This is because a clock is one of the most famous
example of modular arithmetic (sometimes called clock arithmetic) is a system of
where numbers "wrap around" upon reaching a certain value: the modulus.
Now image we have two clocks and we want to install a third one where the hour is the
result of "adding" together the time tracked by the first and the second. This is
extremely easy to do, because we know that upon reaching 12 our modulus will "reset"
the clock count:

```
11 + 10 = 21 mod 12 = 9
```

If you think about that, a clock is nothing more than a device to track time,
which must obey to a set of rules:

* If we add 12 to our clock, we'll get back the same hour we had in the first place. Thus
  we have a "unit" element, which we'll call *u* which doesn't change the result
  of our final computation. 
* We can take two clocks and build a new one where the hour is given by
  the sum of the two. In other terms we have a *binary operation* which we'll call
  *mappend*.


### Sums and Products

* Explain why, by-default we can't do "mempty :: Int".

```
module Main where

import Data.Monoid

newtype MySum = MySum { getSum :: Int } deriving (Show)

instance Monoid MySum where
    mempty = MySum 0
    mappend (MySum x) (MySum y) = MySum (x + y)

newtype MyProd = MyProd { getProd :: Int } deriving (Show)

instance Monoid MyProd where
    mempty = MyProd 0
    mappend (MyProd x) (MyProd y) = MyProd (x * y)

main :: IO ()
main = let s1 = MySum 10
           s2 = MySum 20
           m1 = MyProd 4
           m2 = MyProd 3
       in do
           print . show $ s1 <> s2
           Print . show $ m1 <> m2
```


## A formal definition
A monoid is a structure *(R, ⊕, u)* where *R* is a set, *⊕* is a binary operation on *R*
and *u* is the identity element on *R*, and these laws holds:

```
(r ⊕ s) ⊕ t = r ⊕ (s ⊕ t)
1 ⊕ r = r
r ⊕ 1 = r
```



### Example x: Trees

```
import Data.Monoid

data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Monoid Tree where
    mempty = Leaf
    mappend = undefined
```




### Example x: Strings

Using `mappend` and `mempty` is way too easy for builtin `String`:
```
import Data.Monoid

w1 = "foo"
w2 = "bar"
w3 = w1 <> w2 <> mempty

main = print w3
```

Notice how GHC is smart enough to recognize that we want to call the
`mempty` instance specific for the `String` type.

### Example x: Functions
Finally

## Why do Monoids matter?

After all this digression, you might think: why on earth should I worry
about monoids in my everyday programming activity? This is certainly a
fair question, and I'm going to answer it with just one word: **abstraction**.

## References

* "Don't fear the monad by Brian Beckman" - where the clock example was taken.
* "Introduction to Category Theory by Simmons"

