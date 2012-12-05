-- Card.hs

module Card
  (Suit(..),
   Value(..),
   Card(..),
   suits,
   values,
   card,
   showVerboseCard,
   showVerboseCardPlural,
   showCompactSuit,
   showVerboseSuit,
   showCompactCard,
   deck,
   cowboys,
   oneEyed,
   isAce,
   sameSuit,
   sameSuits,
   sameValue,
   sameValues,
   adjacentValue,
   ascendingValues,
   divide,
   riffle,
   box,
   shuffle)
where

import Maybe

data Number a = N a deriving Show

data Suit     = Spades | Hearts | Clubs | Diamonds     deriving Eq
data Value    = Number Int | Jack | Queen | King | Ace deriving Eq
data Card     = Card Value Suit                        deriving Eq

instance Ord Value where
  (<=) (Number a) (Number b) = (<=) a b
  (<=) (Number _) Jack       = True
  (<=) Jack (Number x)       = (not ((<=) (Number x) Jack))
  (<=) (Number _) Queen      = True
  (<=) Queen (Number x)      = (not ((<=) (Number x) Queen))
  (<=) (Number _) King       = True
  (<=) King (Number x)       = (not ((<=) (Number x) King))
  (<=) (Number _) Ace        = True
  (<=) Ace (Number x)        = (not ((<=) (Number x) Ace))
  (<=) Jack Queen            = True
  (<=) Queen Jack            = (not ((<=) Jack Queen))
  (<=) Jack King             = True
  (<=) King Jack             = (not ((<=) Jack King))
  (<=) Jack Ace              = True
  (<=) Ace Jack              = (not ((<=) Jack Ace))
  (<=) Queen King            = True
  (<=) King Queen            = (not ((<=) Queen King))
  (<=) Queen Ace             = True
  (<=) Ace Queen             = (not ((<=) Queen Ace))
  (<=) King Ace              = True
  (<=) Ace King              = (not ((<=) King Ace))
  (<=) Jack Jack             = True
  (<=) Queen Queen           = True
  (<=) King King             = True
  (<=) Ace Ace               = True

instance Ord Card where
  (<=) (Card a _) (Card b _) = (<=) a b

showCompactVal, showVerboseVal, showVerboseValPlural :: Value -> String
showCompactVal (Number num) = show num
showCompactVal Jack         = "J"
showCompactVal Queen        = "Q"
showCompactVal King         = "K"
showCompactVal Ace          = "A"


showVerboseVal (Number 2)  = "Deuce"
showVerboseVal (Number 3)  = "Trey"
showVerboseVal (Number 4)  = "Four"
showVerboseVal (Number 5)  = "Five"
showVerboseVal (Number 6)  = "Six"
showVerboseVal (Number 7)  = "Seven"
showVerboseVal (Number 8)  = "Eight"
showVerboseVal (Number 9)  = "Nine"
showVerboseVal (Number 10) = "Ten"
showVerboseVal Jack        = "Jack"
showVerboseVal Queen       = "Queen"
showVerboseVal King        = "King"
showVerboseVal Ace         = "Ace"

showVerboseValPlural (Number 6) = (showVerboseVal (Number 6)) ++ "es"
showVerboseValPlural (Number n) = (showVerboseVal (Number n)) ++ "s"
showVerboseValPlural Jack       = "Jacks"
showVerboseValPlural Queen      = "Queens"
showVerboseValPlural King       = "Kings"
showVerboseValPlural Ace        = "Aces"


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

instance Show Card where
  show = showCompactCard

instance Show Value where
  show = showCompactVal

instance Show Suit where
  show = showCompactSuit

suits :: [Suit]
suits = [Spades, Hearts, Clubs, Diamonds]

values :: [Value]
values = [(Number i) | i <- [2..10]] ++ [Jack, Queen, King, Ace]

getValueFromString :: Char -> Maybe Value
getValueFromString '2' = Just (Number 2)
getValueFromString '3' = Just (Number 3)
getValueFromString '4' = Just (Number 4)
getValueFromString '5' = Just (Number 5)
getValueFromString '6' = Just (Number 6)
getValueFromString '7' = Just (Number 7)
getValueFromString '8' = Just (Number 8)
getValueFromString '9' = Just (Number 9)
getValueFromString 'T' = Just (Number 10)
getValueFromString 'J' = Just Jack
getValueFromString 'Q' = Just Queen
getValueFromString 'K' = Just King
getValueFromString 'A' = Just Ace
getValueFromString _ = Nothing

getSuitFromString :: Char -> Maybe Suit
getSuitFromString 'S' = Just Spades
getSuitFromString 'H' = Just Hearts
getSuitFromString 'C' = Just Clubs
getSuitFromString 'D' = Just Diamonds
getSuitFromString _ = Nothing

card :: String -> Maybe Card
card str = let val  = getValueFromString (str !! 0)
               suit = getSuitFromString (str !! 1)
           in if (isNothing val) || (isNothing suit) then Nothing
              else Just (Card (fromJust val) (fromJust suit))

deck :: [Card]
deck = [(Card value suit) | suit <- suits, value <- values]

cowboys :: [Card]
cowboys = [card | card <- deck, (\(Card val _) -> (val == King)) card]

oneEyed :: [Card]
oneEyed = [card | card <- deck, (((Card King Diamonds) == card) || 
                                 ((Card Jack Spades) == card)   ||
                                 ((Card Jack Hearts) == card))]

isAce :: Card -> Bool
isAce (Card val _) = val == Ace

sameSuit :: Card -> Card -> Bool
sameSuit (Card _ suit1) (Card _ suit2) = suit1 == suit2

sameSuits :: [Card] -> Bool
sameSuits ((Card val suit):xs) = ss suit ((Card val suit):xs)
                                 where ss suit [] = True
                                       ss suit ((Card _ csuit):cs)
                                         | suit == csuit = ss suit cs
                                         | otherwise = False

sameValue :: Card -> Card -> Bool
sameValue (Card val1 _) (Card val2 _) = val1 == val2

sameValues :: [Card] -> Bool
sameValues ((Card val suit):xs) = sv val ((Card val suit):xs)
                                  where sv val [] = True
                                        sv val ((Card cval _):cs)
                                          | val == cval = sv val cs
                                          | otherwise = False

adjacentValue :: Card -> Card -> Bool
adjacentValue (Card (Number val1) _) (Card (Number val2) _) = (val1 == (val2 + 1)) || (val1 == (val2 - 1))
adjacentValue (Card Jack _) (Card (Number 10) _) = True
adjacentValue (Card (Number 10) _) (Card Jack _) = True
adjacentValue (Card Jack _) (Card Queen _) = True
adjacentValue (Card Queen _) (Card Jack _) = True
adjacentValue (Card Queen _) (Card King _) = True
adjacentValue (Card King _) (Card Queen _) = True
adjacentValue (Card King _) (Card Ace _) = True
adjacentValue (Card Ace _) (Card King _) = True
adjacentValue (Card Ace _) (Card (Number 2) _) = True
adjacentValue (Card (Number 2) _) (Card Ace _) = True
adjacentValue _ _ = False

-- incompleted at the moment
before :: Value -> Value -> Bool
before Ace (Number 2) = True
before (Number _) Ace = True
before (Number a) (Number b) = (a + 1) == b
before (Number 10) Jack = True
before Jack Queen = True
before Queen King = True
before King Ace = True
before _ _ = False

ascendingValues :: [Card] -> Bool
ascendingValues ((Card val suit):xs) = av val xs
                           where av _ [] = True
                                 av val ((Card cval _):cs)
                                   | val `before` cval = av cval cs
                                   | otherwise = False

divide :: Int -> [Card] -> [[Card]]
divide at deck = unwrap (splitAt at deck)
                 where unwrap (a, b) = [a, b]

-- The riffle shuffle
riffle :: [[Card]] -> [Card]
riffle [a, b] = concat (zipWith (\x -> \y -> [x, y]) a b)
riffle _ = error "deck not halved"

-- The box shuffle
box :: [Card] -> [Card]
box deck = let a = divide 16 deck
               b = divide 18 (a !! 1)
               c = (b !! 1)
           in c ++ (b !! 0) ++ (a !! 0)

shuffle :: [Card] -> ([Card] -> [Card]) -> Int -> [Card]
shuffle deck method thoroughness
  | thoroughness == 0 = deck
  | otherwise         = shuffle (method deck) method (thoroughness - 1)

