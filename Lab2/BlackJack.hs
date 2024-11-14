module BlackJack where

import Cards
import RunGame
import Test.QuickCheck

-- LAB 2A
sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)(Add (Card Jack Spades) Empty))
            , 1 + (size (Add (Card Jack Spades) Empty))
            , 1 + (1 + size Empty)
            , 1 + 1 + 0
            , 2]

-- A hand containing 2 cards (1 of hearts & Queen of spades)
hand1 :: Hand
hand1 = Add (Card (Numeric 1) Hearts)
            (Add (Card Queen Spades) Empty)

-- A hand containing 2 cards (2 of hearts & Jack of spades)
hand2 :: Hand
hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)

-- Hand containing 3 cards
hand3 = Add (Card (Numeric 2) Hearts)(Add
            (Card Jack Spades)(Add 
            (Card Ace Hearts) Empty))

-- Hand containing 3 cards
hand4 = Add (Card King Hearts)(Add
            (Card Jack Spades)(Add 
            (Card Queen Hearts) Empty))

-- Two cards, Jack of Hearts & 5 of Clubs
card5 = Card Jack Hearts
card6 = Card (Numeric 5) Clubs

-- Display a card in the form of RANK OF SUIT
displayCard :: Card -> String
displayCard (Card (Numeric n) s) = show n ++ " of " ++ show s ++ "\n"
displayCard c = show(rank c) ++ " of " ++ show (suit c) ++ "\n"

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
value h | initValue > 21 = initValue - (numberOfAces h * 10)
                | otherwise = initValue
   where initValue = initialvalue h

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

---------
-- Lab 2B

-- Combining two hands
(<+) :: Hand -> Hand -> Hand
(<+) Empty h2 = h2
(<+) (Add c h) h2 = Add c (h <+ h2)

-- Check if the cards are added in the right order
-- ergo, h1 + (h2 + h3) == (h1 + h2) + h3
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool 
prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3

-- Check if the size of the combination of hand1 and hand2 is
-- the same as the size of hand1 + hand2
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size (h1 <+ h2) == size h1 + size h2

-- Function that returns a hand with a full deck of Cards
fulldeck :: Hand
fulldeck = foldr Add Empty deck
   where deck = [Card r s | s <- suits, r <- ranks]
         ranks = [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]
         suits = [Hearts, Spades, Diamonds, Clubs]

-- Take one card from the deck and add to hand
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add c deck) hand = (deck, Add c hand)

-- Draw the hand for the bank
playBank :: Hand -> Hand
playBank bankHand | valueB == 0 = playBank(playBankHelper( playBankHelper(fulldeck bankHand)))
                  | valueB < 16 = playBankHelper 
                  | otherwise = bankHand
   where valueB = value bankHand
         deck = fulldeck

playBankHelper :: Hand -> Hand
playBankHelper deck hand = biggerHand
    where (smallerDeck,biggerHand) = draw deck hand
