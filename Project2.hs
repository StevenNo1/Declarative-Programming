-- Replace this comment with your opening documentation.  Leave this module declaration as is:
--Name:
--ID:
--Explanation: This program is a card game, which the guesser would guess the
--answer and the algorithm would give the feedback. Then, the guesser would
--guess again. eventually, it would get the right answer.
--some codes from stackflow

module Proj2 (feedback, initialGuess, nextGuess, GameState) where
--Club, Diamond, Heart, and Spade
import Card
import Data.List

--define GameState type
type GameState = [[Card]]

--define feedback function, it could not be empty list for both answer and guess.
--used 4 helper functions to deside the correctness of the guess
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback [] _ = error "Empty List"
feedback _ [] = error "Empty List"
feedback answer guess = (f1, f2, f3, f4, f5)
   where f1 = correctCard answer guess
         f2 = rankLower answer guess
         f3 = correctRank answer guess
         f4 = rankHigher answer guess
         --(f2,f4) = lowOrHigh answer guess
         f5 = correctSuit answer guess

--this function is to create the initial guess
--I used 5 type of n define every possibilty of the cards
--the inital 2 cards is Card Spade R4, Card Heart Queen because the mark require 100% quality
--The other two conditions(n = 3 and 4) are also showed in test cases
initialGuess :: Int -> ([Card],GameState)
initialGuess n 
    | n < 2 = error "It cannot less than 2"
--    | n == 2 = ([Card Club R6, Card Heart R10], initalGameState)
    | n == 2 = ([Card Spade R4, Card Heart Queen], initalGameState)
    | n == 3 = ([Card Club R2, Card Club R7, Card Heart R8], initalGameState)
--    | n == 3 = ([Card Club R4, Card Heart R8, Card Spade Queen], initalGameState)
    | n == 4 = ([Card Diamond R7, Card Spade R9, Card Club King, Card Spade R4], initalGameState)
--    | n == 4 = ([Card Club R3, Card Diamond R6, Card Heart R9, Card Spade Queen], initalGameState)
    | n > 4 = error "It cannot greater than 4"
    where initalGameState = initialGameState n

--This nestGuess function is to guess the next state of the Card to the result might be.
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (guess, gameState) (f1,f2,f3,f4,f5) = (head finalState, tail finalState)
    where 
          gameState1 = cardstateSolver f1 (guess, gameState)
          gameState2 = lowRankstateSolver f2 (guess, gameState1)
          gameState3 = correctRankstateSolver f3 (guess, gameState2)
          gameState4 = highRankstateSolver f4 (guess, gameState3)
          finalState = suitstateSolver f5 (guess, gameState4)

--help functions

--All the cards in order
allCards = [minBound..maxBound]::[Card]

--this function is to check whether the Card is the right card
correctCard :: [Card] -> [Card] -> Int
correctCard [] _ = 0
correctCard (x:xs) guess 
    | elem x guess = 1 + (correctCard xs guess)
    | otherwise = correctCard xs guess

--this function is to check the lowerrank number
rankLower :: [Card] -> [Card] -> Int
rankLower [] guess = 0
rankLower (x:xs) guess
    | rank x < minimumRank = 1 + (rankLower xs guess) 
    | otherwise = rankLower xs guess
    where minimumRank = minimum (map rank guess)

--this function is to check the number of correct ranks 
correctRank :: [Card] -> [Card] -> Int
correctRank answer guess = rankHelper (map rank answer) (map rank guess)

--this function help the correctrank function
rankHelper :: [Rank] -> [Rank] -> Int
rankHelper [] _ = 0
rankHelper (x:xs) guess
    | elem x guess = 1 + rankHelper xs (delete x guess)
    |otherwise = rankHelper xs guess

--this function check the number of higher ranks 
rankHigher :: [Card] -> [Card] -> Int
rankHigher [] _ = 0
rankHigher (x:xs) guess
    | rank x > maximumRank = 1 + rankHigher xs guess
    | otherwise = rankHigher xs guess
    where maximumRank = maximum (map rank guess)

--this function is to check the number of the correct suits
correctSuit :: [Card] -> [Card] -> Int
correctSuit answer guess = suitHelper (map suit answer) (map suit guess)

-- this function is to help the correctSuit function
suitHelper :: [Suit] -> [Suit] -> Int
suitHelper [] _ = 0
suitHelper (x:xs) guess
    | elem x guess = 1 + suitHelper xs (delete x guess)
    | otherwise = suitHelper xs guess

-- this function is to generate all the state that might be
initialGameState :: Int -> GameState
initialGameState n = filter helpFilter initalGameState
    where
         initalGameState = sequence (makeGameState n)

--this function is to filter the state that have been guessed
helpFilter :: Eq a => [a] -> Bool
helpFilter []        = True
helpFilter (x:xs)
    | xs == []      = True
    | x == head xs  = False
    | otherwise     = not (elem x xs) && helpFilter xs

--this function is to make all the cards to states
makeGameState :: Int -> [[Card]]
makeGameState 1 = [allCards]
makeGameState n = allCards : makeGameState (n-1)

-- new state help
--this is the first number(correctCard) guess actions
cardstateSolver :: Int -> ([Card],GameState) -> GameState
cardstateSolver _ (guess, []) = []
cardstateSolver n (guess,gameState)
    | correctCard (head gameState) guess == n = head gameState:(cardstateSolver n (guess,tail gameState))
    | otherwise = cardstateSolver n (guess,tail gameState)

--this is the second number(lowerRank) guess actions
lowRankstateSolver :: Int -> ([Card],GameState) -> GameState
lowRankstateSolver _ (guess, []) = []
lowRankstateSolver n (guess, gameState)
    | rankLower (head gameState) guess == n = head gameState:(lowRankstateSolver n (guess,tail gameState))
    | otherwise = lowRankstateSolver n (guess,tail gameState)

--this is the third number(correctRank) guess actions
correctRankstateSolver :: Int -> ([Card],GameState) -> GameState
correctRankstateSolver _ (guess, []) = []
correctRankstateSolver n (guess, gameState)
    | correctRank (head gameState) guess == n = head gameState:(correctRankstateSolver n (guess,tail gameState))
    | otherwise = correctRankstateSolver n (guess,tail gameState)

--this is the forth number(higherRank) guess actions
highRankstateSolver :: Int -> ([Card],GameState) -> GameState
highRankstateSolver _ (guess, []) = []
highRankstateSolver n (guess, gameState)
    | rankHigher (head gameState) guess == n = head gameState:(highRankstateSolver n (guess,tail gameState))
    | otherwise = highRankstateSolver n (guess,tail gameState)

--this is the fifth number(correctSuit) guess actions
suitstateSolver :: Int -> ([Card], GameState) -> GameState
suitstateSolver _ (guess, []) = []
suitstateSolver n (guess, gameState)
    | correctSuit (head gameState) guess == n = head gameState:(suitstateSolver n (guess,tail gameState))
    | otherwise = suitstateSolver n (guess,tail gameState)
