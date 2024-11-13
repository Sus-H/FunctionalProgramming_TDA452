module BlackJack where

import Cards
import RunGame
-- import Test.QuickCheck

-- Display a card in the form of RANK OF SUIT
displayCard :: Card -> String
displayCard c | take 8 (show(rank c)) == "Numeric " 
                 = drop 8 (show(rank c)) ++ " of " ++ show (suit c) ++ "\n"
              | otherwise = show(rank c) ++ " of " ++ show (suit c) ++ "\n"

-- Display all cards in one hand
display :: Hand -> String
display Empty = []
display (Add c h) = displayCard c ++ display h

-- Calculates the total value of a hand assuming Rank Ace = 11
initialvalue :: Hand -> Integer
initialvalue Empty = 0
initialvalue (Add c h) = valueRank (rank c) + initialvalue h

-- Returns the value for each rank
valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Ace         = 11
valueRank _           = 10

-- The final value of a hand assuming the right value for Ace (1 or 11)
value :: Hand -> Integer
value Empty = 0
value h | initialvalue h > 21 = initialvalue h - (numberOfAces h * 10)
                | otherwise = initialvalue h

-- Determines if the value of Ace sould be 1 or 11
-- valueOfAce :: Hand -> Integer (to make it clearer for us)
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add c h) | rank c == Ace = 1 + numberOfAces h
                       | otherwise = 0 + numberOfAces h

-- Check if a hand is bust, over 21 points
gameOver :: Hand -> Bool
gameOver h = value h > 21

-- Check who is the winner given hand for bank and guest
winner :: Hand -> Hand -> Player
winner guestHand bankHand | gameOver bankHand && gameOver guestHand = Bank
                          | gameOver bankHand                       = Guest
                          | gameOver guestHand                      = Bank
                          | value bankHand >= value guestHand       = Bank
                          | otherwise                               = Guest

hand2 :: Hand
hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)

sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            -- ... add the remaining steps here
            , 2]