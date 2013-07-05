module Main where

import Data.Monoid

sumIt :: (Num a) => a -> a
sumIt x = x + x

squareIt :: (Num a) => a -> a
squareIt x = x * x


-- This bit shows that if we have a (Monoid b) as
-- the second argument we can use plain function
-- composition with mappend.
f :: (Num a) => a -> Sum a
f x = Sum (x + x)

g :: (Num a) => a -> Sum a
g x = Sum (x * x)

k :: (Num a) => a -> Sum a
k = f <> g

-- Plain application of "." vs Endo mappend
h :: (Num a) => a -> a
h = squareIt . sumIt

h' :: (Num a) => Endo a
h' = Endo squareIt <> Endo sumIt

main :: IO ()
main = do
    print $ h 10
    print $ appEndo h' 10
