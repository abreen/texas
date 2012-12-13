-- Poker.hs

module Poker
  (Hand(..),
   hand,
   deal,
   dealBoard)
where

import Card
import Maybe
import Data.List (sort, subsequences)

data Hand = NullHand | HighCard Card | Pair [Card] | TwoPair [Card] |
            ThreeOfAKind [Card] | Straight [Card] |
            Flush [Card] | FullHouse [Card] |
            FourOfAKind [Card] | StraightFlush [Card] |
            RoyalFlush [Card] deriving (Eq, Ord)

-- Returns True if the five cards constitute a royal flush
isRoyalFlush :: [Card] -> Bool
isRoyalFlush cs =
  let sorted = sort cs in (sameSuits sorted) && (ascending sorted)
                          && (isAce (last sorted))

-- Returns the best hand represented by a list of cards
hand :: [Card] -> Hand
hand cs =
  case cs of
    []              -> NullHand
    [a]             -> HighCard a
    [a, b]          -> if sameValue a b then Pair cs
                       else max (hand [a]) (hand [b])
    [a, b, c]       -> if sameValues cs then ThreeOfAKind cs
                       else maximum (map hand ((init.subsequences) cs))
    [a, b, c, d]    -> if sameValues cs then FourOfAKind cs
                       else if ((sameValue a b) && (sameValue c d))
                               || ((sameValue a c) && (sameValue b d))
                            then TwoPair cs
                            else maximum (map hand ((init.subsequences) cs))
    [a, b, c, d, e] -> if isRoyalFlush cs then RoyalFlush cs
                       else if ascending cs && sameSuits cs
                            then StraightFlush cs
                            else if (sameValue a b) && (sameValues [c, d, e])
                                 then FullHouse cs
                                 else if sameSuits cs then Flush cs
                                      else if ascending cs
                                           then Straight cs
                                           else maximum (map hand
                                                ((init.subsequences) cs))
    _ -> maximum (map hand [h | h <- (subsequences cs), (length h) < 6])

seqDeal :: Int -> Int -> [Card] -> Int -> [[Card]] -> ([Card], [[Card]])
seqDeal players num (d:deck) player hands
  | all (((==) num).length) hands = ((d:deck), hands)
  | otherwise = seqDeal players num deck (mod (player+1) players) (addToPile hands d player 0)
                where addToPile (hand:hands) card player i
                        | i == player = ((card:hand):hands)
                        | otherwise   = hand : (addToPile hands card player (i+1))

-- Deals variable hand sizes to variable number of players
deal :: [Card] -> Int -> Int -> ([Card], [[Card]])
deal deck num players = seqDeal players num deck 0 (prepareLists players []) 
                        where prepareLists n ls
                                | n > 0     = prepareLists (n-1) ([]:ls)
                                | otherwise = ls

-- Deals a flop, turn and river from a deck
dealBoard :: [Card] -> ([Card], [Card], Card, Card)
dealBoard cards = (rest, flop, turn, river)
                  where a = tail cards            
                        flop = take 3 a
                        b = snd (splitAt 4 a)
                        turn = head b
                        c = snd (splitAt 2 b)
                        river = head c
                        rest = tail c

