data List a = Nil | Cons (a, List a) deriving (Show)

listSum :: (Num a) => List a -> a
listSum Nil = 0
listSum (Cons (x, xs)) = x + listSum xs
