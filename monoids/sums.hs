
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
           print . show $ m1 <> m2
