module HW2 where

import Data.List
import Data.Char
import Words

type Hand = [Char]
type Template = String
type STemplate = String

-- Exercise 1

formableBy :: String -> Hand -> Bool
formableBy [] _ = True
formableBy (x:xs) hand
    | x `elem` hand     = formableBy xs $ delete x hand
    | x `notElem` hand  = False

-- Exercise 2

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (\word -> formableBy word hand) allWords

-- Exercise 3

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate [] _ [] = True
wordFitsTemplate (t:ts) hand (w:ws)
    | (t == '?') && (w `elem` hand) = let newHand = delete w hand
                                      in wordFitsTemplate ts newHand ws
    | (t == w)                      = wordFitsTemplate ts hand ws
    | otherwise                     = False
wordFitsTemplate _ _ _              = False


-- Exercise 4

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate template hand = filter (\word -> wordFitsTemplate template hand word) allWords

-- Exercise 5

scrabbleValueWord :: String -> Int
scrabbleValueWord = foldl (\val c -> val + scrabbleValue c) 0

-- Exercise 6

bestWords :: [String] -> [String]
bestWords = bestWords' []
    where
        bestWords' :: [String] -> [String] -> [String]
        bestWords' acc [] = acc
        bestWords' [] (x:xs) = bestWords' [x] xs
        bestWords' acc (w:ws) =
            let currentValue = scrabbleValueWord $ head acc
                wordValue    = scrabbleValueWord w
            in case compare currentValue wordValue of
                    LT -> bestWords' [w] ws 
                    EQ -> bestWords' (w : acc) ws
                    GT -> bestWords' acc ws


-- Exercise 7

scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate template word = (letterMultiplier 0 template word) * (wordMultiplier template)
    where
        letterMultiplier acc [] [] = acc
        letterMultiplier acc (t:ts) (w:ws)
            | 'D' == t = let newAcc = (acc + (scrabbleValue w * 2))
                         in letterMultiplier newAcc ts ws

            | 'T' == t = let newAcc = (acc + (scrabbleValue w * 3))
                         in letterMultiplier newAcc ts ws

            | otherwise = let newAcc = (acc + scrabbleValue w)
                          in letterMultiplier newAcc ts ws
        
        wordMultiplier w
            | '2' `elem` w  = 2
            | '3' `elem` w  = 3
            | otherwise     = 1
            

