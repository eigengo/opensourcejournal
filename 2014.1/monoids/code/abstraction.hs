
import Data.Monoid

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Eq, Show)

genericFold :: (Monoid a) => Tree a -> a
genericFold t = go t mempty
  where
    go :: (Monoid a) => Tree a -> a -> a
    go (Leaf v) acc = v <> acc
    go (Node v t1 t2) acc = v <> (go t1 acc) <> (go t2 acc)


treeOfSum :: Tree (Sum Int)
treeOfSum = Node (Sum 5)
             (Node (Sum 6) (Leaf (Sum 8)) (Leaf (Sum 4)))
             (Leaf (Sum 2))
            
treeOfProd :: Tree (Product Int)
treeOfProd = Node (Product 5)
              (Node (Product 6) (Leaf (Product 8)) (Leaf (Product 4)))
              (Leaf (Product 2))

treeOfLists :: Tree [Int]
treeOfLists = Node [5, 5]
               (Node [] (Leaf [1,2]) (Leaf [4,90]))
               (Leaf mempty)

f1 :: (Num a) => a -> a
f1 = (2*)

treeOfEndos :: (Num a) => Tree (Endo a)
treeOfEndos = Node (Endo f1)
               (Node (Endo id) (Leaf (Endo f1)) (Leaf (Endo f1)))
               (Leaf (Endo f1))


main :: IO ()
main = do
  print . show $ genericFold treeOfSum
  print . show $ genericFold treeOfProd
  print . show $ genericFold treeOfLists
  print . show $ appEndo (genericFold treeOfEndos) 8

