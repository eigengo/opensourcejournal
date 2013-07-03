
module Main where

import Data.Monoid

newtype MySum = MySum { getSum :: Int } deriving (Show, Eq)

instance Monoid MySum where
    mempty = MySum 0
    mappend (MySum x) (MySum y) = MySum (x + y)
    
isMonoid :: MySum -> Bool
isMonoid s1@(MySum _) = prop1 && prop2 && prop3
  where
    prop1 = let s2 = MySum 10
                s3 = MySum 5
            in (s1 <> (s2 <> s3)) == ((s1 <> s2) <> s3)
    prop2 = (mempty <> s1) == s1
    prop3 = (s1 <> mempty) == s1

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
