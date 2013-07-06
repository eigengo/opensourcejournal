import Data.Monoid

w1 :: String
w1 = "foo"

w2 :: [Char]
w2 = "bar"

w3 :: String
w3 = w1 <> w2 <> mempty

main :: IO ()
main = print w3
