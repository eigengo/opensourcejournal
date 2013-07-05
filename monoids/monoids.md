
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
  *⊕*.

### A formal definition
A monoid is a structure *(R, ⊕, u)* where *R* is a set, *⊕* is a binary operation on *R*
and *u* is the identity element on *R*, and these laws holds:

```
(r ⊕ s) ⊕ t = r ⊕ (s ⊕ t)
u ⊕ r = r
r ⊕ u = r
```

Even if those laws might seem daunting at first, they are quite simple:

* The first one says that no matter how we "sum" things, we'll always
  yield the same result
* The last twos just say that no matter when we "sum" the unit element
  *u*, we'll never affect the overall result.

Sounds familiar?


### Sums and Products

If the answer was yes, it's just because I played a dirty trick on
you when I introduced the word "sum" in the description. For the
rest of the article, we'll succinctly describe our ideas through
Haskell, a purely functional programming language you might be
familiar with. Not only is Haskell a fantastic tool for things
like that, but its math roots causes Monoids to be part of the
language itself! If you are not familiar with Haskell don't worry,
we'll explain things along the way. As said, in Haskell a Monoid
is a first class citizen, and its definition as well as useful
functions live in a module called ``Data.Monoid`` that you can
import to play around. Let's take a look at this snippet of code:


```
module Main where

import Data.Monoid

newtype MySum = MySum { getSum :: Int } deriving (Show, Eq)

instance Monoid MySum where
    mempty = MySum 0
    mappend (MySum x) (MySum y) = MySum (x + y)

```

It might seems a lot to absorb at first, but it's easier
than it looks. First of all, we want to define a new
datatype called ``MySum``, and we want
also to make it a Monoid. We do this through Haskell's
typeclasses, a delightful way to express particular
properties our datatypes should stick to. A ``Monoid``
typeclass, which lives in ``Data.Monoid`` expects us to
fully implement at least two functions, ``mempty`` and
``mappend``. Sounds familiar? Let's check ``MySum`` is
actually a ``Monoid``:

```
-- <> is just the infix version of mappend
isMonoid :: MySum -> Bool
isMonoid s1@(MySum _) = prop1 && prop2 && prop3
  where
    prop1 = let s2 = MySum 10
                s3 = MySum 5
            in (s1 <> (s2 <> s3)) == ((s1 <> s2) <> s3)
    prop2 = (mempty <> s1) == s1
    prop3 = (s1 <> mempty) == s1
```

If you try this in the REPL you'll see ``True`` displayed.
Granted, this is a sort of "poor-man-proof", and in case
we needed more power we probably would have used a proper
tool like ``QuickCheck`` (which I recommend). Have you
noticed? We have just "proved" that the laws hold, let's
rewrite them with our Haskell notation:

```
(r <> s) <> t = r <> (s <> t)
mempty <> r = r
r <> mempty = r
```

Et voilà, the Monoid is served! We can of course do the
same for multiplication:


```
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
           print . show $ m1 <> m2
```

The implementation is exactly the same, the only thing which
changes is the `newtype` name and the binary operation. As
this regards, when I started looking at Monoids in Haskell
I was confused on why I couldn't simply do something like
this in the REPL:

```
let x = mempty :: Int
<interactive>:3:9:
    No instance for (Monoid Int) arising from a use of `mempty'
    Possible fix: add an instance declaration for (Monoid Int)
    In the expression: mempty :: Int
    In an equation for `x': x = mempty :: Int
```

The problem is that how can possibly the compiler gives us back
a Monoid if we haven't specified a binary operation! An Int
can be used to form a Monoid around the `+` operation (already
defined in Haskell as `Sum`) or the `*` operation (already
defined in Haskell as `Prod`). For other types, where there is
no such ambiguity, everything works as expected:

```
let y = mempty :: [Int]

> []
```


### Strings and Lists

The last example unravel that, maybe unsurprisingly, lists are
Monoid too, where its definition is:

```
instance Monoid [a] where
        mempty  = []
        mappend = (++)
```

Due to the fact that in Haskell a `String` is a type synonym
for `[Char]`, it comes as no surprise that strings are Monoid too:

```
import Data.Monoid

w1 :: String
w1 = "foo"

w2 :: [Char]
w2 = "bar"

w3 :: String
w3 = w1 <> w2 <> mempty

main :: IO ()
main = print w3
```

I've put the type signature on purpose, to show how Haskell treat
indifferently `String` and `[Char]`. Also notice how GHC is
smart enough to recognize that we want to call the `mempty`
instance specific for the `String` type.

### Functions
The last example I'll show you is about plain simple functions:
it turns out that functions form a monoidal category closed on
function composition! If you think about that, composing a
function is exactly the `mappend` operation we encountered for
the other examples. But what about the identity element? Well,
as you probably know, there is a function which acts exactly
like that... is the `id` function!

```
id :: a -> a
```

As a matter of fact, `Data.Monoid` already defines an instance
of `Monoid` as such:

```
instance Monoid b => Monoid (a -> b) where
        mempty _ = mempty
        mappend f g x = f x `mappend` g x
```

But the problem here is that it assumes `b` should be a
`Monoid`. How can we make it work for each function, regardless
from the signature? One possibility is to use `Endo`, cryptically
defined as "the endomorphism under function composition": we won't
dig too much into it, all you need to know is that it is a
morphism (a structure-preserving transformation) from an entity to
itself. Let's assume we have definited two toy functions, `squareIt`
and `sumIt` this way:

```
sumIt :: (Num a) => a -> a
sumIt x = x + x

squareIt :: (Num a) => a -> a
squareIt x = x * x
```

We could compose the two functions the usual way, or "mappending" them
using `Endo`, yielding the same result in both cases:

```
import Data.Monoid

h :: (Num a) => a -> a
h = squareIt . sumIt

h' :: (Num a) => Endo a
h' = Endo squareIt <> Endo sumIt

-- We use appEndo to "extract" the final function.
main :: IO ()
main = do
    print $ h 10
    print $ appEndo h' 10
```

If you try this code in the REPL, you'll see the results are the same.

## Why do Monoids matter?

After all this digression, you might think: why on earth should I worry
about monoids in my everyday programming activity? This is certainly a
fair question, and I'm going to answer it with just one word: **abstraction**.
Dealing with data structures we know are Monoids allow us to exploit this
information to write generic code; after all, we are guaranteed (by the
type system) that we can always "mappend" two monoids together or generate
the unit element for a given type which is also a Monoid. I'll conclude
this article showing you a neat trick: we'll write a generic traversal
function on a tree, where we'll store a monoid into each node; we then
fold the tree accumulating the values as we proceed. Programming with
recursive data structure is also called _origami programming_:

```

```

## References

* "Don't fear the monad by Brian Beckman" - where the clock example was taken.
* "Introduction to Category Theory by Simmons"
