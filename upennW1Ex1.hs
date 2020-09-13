{-# OPTIONS_GHC -Wall #-}

-- exercise 1

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = reverse (toDigitsRev n)

-- exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x : []) = [x]
doubleEveryOther xs = doubleEveryOther (init (init xs)) ++ (last (init xs) * 2 : last xs : [])

-- exercise 3

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits xs = (sum (toDigits (last xs))) + sumDigits (init xs)

-- exercise 4

validate :: Integer -> Bool
validate d
  | sumDigits (doubleEveryOther (toDigits d)) `mod` 10 == 0 = True
  | otherwise                                               = False
