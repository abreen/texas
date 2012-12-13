-- Show.hs

module Show
  (showCompactVal,
   showVerboseVal,
   showVerboseValPlural,
   showCompactSuit,
   showVerboseSuit,
   showCompactCard,
   showVerboseCard,
   showVerboseCardPlural,
   showHand)
where

import Card
import Poker

showCompactVal, showVerboseVal, showVerboseValPlural :: Value -> String
showCompactVal Two   = "2"
showCompactVal Three = "3"
showCompactVal Four  = "4"
showCompactVal Five  = "5"
showCompactVal Six   = "6"
showCompactVal Seven = "7"
showCompactVal Eight = "8"
showCompactVal Nine  = "9"
showCompactVal Ten   = "T"
showCompactVal Jack  = "J"
showCompactVal Queen = "Q"
showCompactVal King  = "K"
showCompactVal Ace   = "A"

showVerboseVal Two   = "Deuce"
showVerboseVal Three = "Trey"
showVerboseVal Four  = "Four"
showVerboseVal Five  = "Five"
showVerboseVal Six   = "Six"
showVerboseVal Seven = "Seven"
showVerboseVal Eight = "Eight"
showVerboseVal Nine  = "Nine"
showVerboseVal Ten   = "Ten"
showVerboseVal Jack  = "Jack"
showVerboseVal Queen = "Queen"
showVerboseVal King  = "King"
showVerboseVal Ace   = "Ace"

showVerboseValPlural Six = (showVerboseVal Six) ++ "es"
showVerboseValPlural n   = (showVerboseVal n) ++ "s"

showCompactSuit, showVerboseSuit :: Suit -> String
showCompactSuit Spades   = "♠"
showCompactSuit Hearts   = "♥"
showCompactSuit Clubs    = "♣"
showCompactSuit Diamonds = "♦"

showVerboseSuit Spades   = "Spades"
showVerboseSuit Hearts   = "Hearts"
showVerboseSuit Clubs    = "Clubs"
showVerboseSuit Diamonds = "Diamonds"


showCompactCard, showVerboseCard, showVerboseCardPlural :: Card -> String
showCompactCard (Card val suit) = (showCompactVal val) ++ 
                                  (showCompactSuit suit)

showVerboseCard (Card val suit) = (showVerboseVal val) ++ " of " ++
                                  (showVerboseSuit suit)

showVerboseCardPlural (Card val suit) = (showVerboseValPlural val)


{- Default styles for when converting values, suits and
 - cards to strings. -}
instance Show Value where
  show = showCompactVal
  
instance Show Suit where
  show = showCompactSuit

instance Show Card where
  show = showCompactCard


showHand :: Hand -> String
showHand (HighCard c)       = "High Card (" ++ (showVerboseCard c) ++ ")"
showHand (Pair cs)          = "Pair of " ++ (showVerboseCardPlural (head cs))
showHand (TwoPair cs)       = (showVerboseCardPlural (cs !! 0))
                              ++ " and " ++ (showVerboseCardPlural (cs !! 2))
showHand (ThreeOfAKind cs)  = "Three " ++ (showVerboseCardPlural (head cs))
showHand (Straight cs)      = "Straight (" ++ (show cs) ++ ")"
showHand (Flush cs)         = "Flush ("  ++ (show cs) ++ ")"
showHand (FullHouse cs)     = "Full House ("  ++ (show cs) ++ ")"
showHand (FourOfAKind cs)   = "Four " ++ (showVerboseCardPlural (head cs))
showHand (StraightFlush cs) = "Straight Flush (" ++ (show cs) ++ ")"
showHand (RoyalFlush ((Card _ suit):cs)) =
  "Royal Flush in " ++ (showVerboseSuit suit)

instance Show Hand where
  show = showHand

