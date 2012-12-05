-- Poker.hs

module Poker
  (hand,
   multiHand,
   deal,
   dealBoard)
where

import Card
-- import Stats
import Maybe
import List

data Hand = HighCard Card | Pair [Card] | TwoPair [Card] | ThreeOfAKind [Card] | Straight [Card] | Flush [Card] | FullHouse [Card] | FourOfAKind [Card] | StraightFlush [Card] | RoyalFlush [Card] deriving (Eq, Ord)


showHand :: Hand -> String
showHand (HighCard c)  = "High Card (" ++ (showVerboseCard c) ++ ")"
showHand (Pair cs) = "Pair of "  ++ (showVerboseCardPlural (head cs))
showHand (TwoPair cs)  = (showVerboseCardPlural (cs !! 0)) ++ " and " ++ (showVerboseCardPlural (cs !! 2))
showHand (ThreeOfAKind cs)  = "Three "  ++ (showVerboseCardPlural (head cs))
showHand (Straight cs)  = "Straight ("  ++ (show cs) ++ ")"
showHand (Flush cs)  = "Flush ("  ++ (show cs) ++ ")"
showHand (FullHouse cs)  = "Full House, " ++ (showVerboseCardPlural (head (fst (tupled cs)))) ++ " full of " ++ (showVerboseCardPlural (head (snd (tupled cs))))
                           where tupled xs
                                   | sameValues (take 3 (sort xs)) = ((take 3 (sort xs)), (snd (splitAt 3 (sort xs))))
                                   | otherwise = ((snd (splitAt 2 (sort xs))), (take 2 (sort xs)))
showHand (FourOfAKind cs)  = "Four "  ++ (showVerboseCardPlural (head cs))
showHand (StraightFlush cs)  = "Straight Flush ("  ++ (show cs) ++ ")"
showHand (RoyalFlush ((Card _ suit):cs)) = "Royal Flush in " ++ (showVerboseSuit suit)

instance Show Hand where
  show = showHand

isRoyalFlush :: Card -> Card -> Card -> Card -> Card -> Bool
isRoyalFlush a b c d e = let cards = sort [a, b, c, d, e] in
                         (sameSuits cards) && (ascendingValues cards)
                         && (isAce (last cards))

subseq :: [a] -> [[a]]
subseq [] = []
subseq (x:xs) = [x] : foldr f [] (subseq xs)
                where f ys r = ys : (x : ys) : r

properSubseq :: [a] -> [[a]]
properSubseq = init.subseq

hand :: [Card] -> Maybe Hand
hand = hand'.sort
hand' [a] = Just (HighCard a)
hand' [a, b]
  | sameValue a b = Just (Pair [a, b])
  | otherwise     = max (hand' [a]) (hand' [b])
hand' [a, b, c]
  | sameValues [a, b, c] = Just (ThreeOfAKind [a, b, c])
  | otherwise = maximum (map hand' (properSubseq [a, b, c]))
hand' [a, b, c, d]
  | sameValues [a, b, c, d] = Just (FourOfAKind [a, b, c, d])
  | (sameValue a b) && (sameValue c d) = Just (TwoPair [a, b, c, d])
  | (sameValue a c) && (sameValue b d) = Just (TwoPair [a, b, c, d])
  | otherwise = maximum (map hand' (properSubseq [a, b, c, d]))
hand' [a, b, c, d, e]
  | (isRoyalFlush a b c d e) = Just (RoyalFlush [a, b, c, d, e])
  | ascendingValues [a, b, c, d, e] &&
    sameSuits [a, b, c, d, e] = Just (StraightFlush [a, b, c, d, e])
  | (sameValue a b) && (sameValues [c, d, e]) ||
    (sameValues [a, b, c]) && (sameValue d e) = Just (FullHouse [a, b, c, d, e])
  | sameSuits [a, b, c, d, e] = Just (Flush [a, b, c, d, e])
  | ascendingValues [a, b, c, d, e] = Just (Straight [a, b, c, d, e])
  | otherwise = maximum (map hand' (properSubseq [a, b, c, d, e]))
hand' _ = Nothing

-- Poker deals
seqDeal :: Int -> Int -> [Card] -> Int -> [[Card]] -> ([Card], [[Card]])
seqDeal players num (d:deck) player hands
  | all (((==) num).length) hands = ((d:deck), hands)
  | otherwise = seqDeal players num deck (mod (player+1) players) (addToPile hands d player 0)
                where addToPile (hand:hands) card player i
                        | i == player = ((card:hand):hands)
                        | otherwise   = hand : (addToPile hands card player (i+1))

deal :: [Card] -> Int -> Int -> ([Card], [[Card]])
deal deck num players = seqDeal players num deck 0 (prepareLists players []) 
                        where prepareLists n ls
                                | n > 0   = prepareLists (n-1) ([]:ls)
                                | otherwise = ls

-- Maximizes a hand of more than 5 cards
multiHand :: [Card] -> Hand
multiHand x = maximum (map (fromJust.hand) ([i | i <- (subseq x), (length i) < 6]))

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

