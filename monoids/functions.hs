module Main where

import Data.Monoid

sumIt :: (Num a) => a -> a
sumIt x = x + x

squareIt :: (Num a) => a -> a
squareIt x = x * x

-- The cool bit now is that, instead of composing the two functions with
-- ".", we can do the same with mappend, "<>".
h :: (Num a) => a -> a
h = sumIt . squareIt

h' :: (Num a) => Endo a
h' = Endo sumIt <> Endo squareIt

f :: (Num a) => a -> Sum a
f x = Sum (x + x)

g :: (Num a) => a -> Sum a
g x = Sum (x * x)

k :: (Num a) => a -> Sum a
k = f <> g

main :: IO ()
main = do
    print $ appEndo h' 10
    print $ k 3
