module HW1 where

-- Exercise 1

lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit number =
    let dropped = lastDigit number
    in (number - dropped) `div` 10

-- Exercise 2

toDigits :: Integer -> [Integer]
toDigits num
    | num > 0  = toDigits (num `div` 10) ++ [num `mod` 10]
    | num <= 0 = []

-- Exercise 3

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = foldr doubler [] x
    where
        doubler num acc
            | (length acc + 1) `mod` 2 == 0  = num * 2 : acc 
            | otherwise                      = num : acc

-- Exercise 4

sumDigits :: [Integer] -> Integer
sumDigits x = foldl (\acc num -> acc + (sumList num)) 0 x
    where 
        sumList 0 = 0
        sumList num = (lastDigit num) + (sumList $ dropLastDigit num)

-- Exercise 5

validate :: Integer -> Bool 
validate x =
    let sum = sumDigits . doubleEveryOther $ toDigits x
    in sum `rem` 10 == 0


-- Tower of Hanoi

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 _ _ _ = []
hanoi num a b c = (hanoi (num - 1) a b c) ++ [(a, c), (a,b), (c,b)]

