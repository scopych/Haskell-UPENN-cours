-- week 1 exercise 5

type Peg = String
type Move = (Peg,Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 1 = [(a,c)]
  | otherwise = hanoi (n - 1) a c b ++ hanoi 1 a b c ++ hanoi (n - 1) b a c

