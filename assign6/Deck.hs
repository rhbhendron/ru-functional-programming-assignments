module Deck where

import Data.List

data Suit  = Clubs | Diamonds | Hearts | Spades       deriving (Eq,Ord,Enum)
data Value = Ace | Numeral Int | Jack | Queen | King  deriving (Eq,Ord)
data Card  = Card { suit :: Suit, value :: Value }    deriving (Eq,Ord)

-- if you have trouble with Unicode, change these definitions as indicated (or fix your locale!)
instance Show Suit where
  show Clubs    = "♣" -- "C"
  show Diamonds = "♦" -- "D"
  show Hearts   = "♥" -- "H"
  show Spades   = "♠" -- "S"
instance Show Value where
  show Ace   = "A"
  show King  = "K"
  show Queen = "Q"
  show Jack  = "J"
  show (Numeral n) = show n
instance Show Card where
  show (Card {suit=s, value=v}) = show s ++ show v

hand :: [Card]
hand = [Card Hearts Queen, Card Spades (Numeral 3), Card Clubs (Numeral 3), Card Hearts (Numeral 7), Card Spades Ace]

shuffledDeck :: [Card]
shuffledDeck = deck 5

shuffledDeckValueKey :: [(Value,Card)]
shuffledDeckValueKey = [ (value card, card) | card <- shuffledDeck ]

shuffledDeckSuitKey :: [(Suit,Card)]
shuffledDeckSuitKey = [ (suit card, card) | card <- shuffledDeck ]

deck :: Int -> [Card]
deck seed = [ Card (toEnum h) (toVal (v+1)) | i <- [1..52], let r = 37*(i+seed) `mod` 52, let h = r `mod` 4, let v = r `div` 4 ]
  where toVal 1  = Ace
        toVal 11 = Jack
        toVal 12 = Queen
        toVal 13 = King
        toVal n  = Numeral n

sortCards :: [Card] -> [Card]
sortCards = concat
          . map (sortOn value)
          . groupBy (\x y->suit x == suit y)
          . sortOn suit
