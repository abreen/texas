-- Card.hs

module Card
  (Suit(..),
   Value(..),
   Card(..),
   suits,
   values,
   card,
   deck,
   cowboys,
   oneEyed,
   isAce,
   sameSuit,
   sameSuits,
   sameValue,
   sameValues,
   ascending,
   riffle,
   box,
   shuffle)
where

data Suit  = Spades | Hearts | Clubs | Diamonds     deriving Eq
data Value = Two | Three | Four | Five | Six |
             Seven | Eight | Nine | Ten | Jack |
             Queen | King | Ace                     deriving (Eq, Ord, Enum)
data Card  = Card Value Suit                        deriving Eq

{- The standard order of cards is determined by their value.
 - There is one special case: an Ace of any suit can be considered
 - "low" or "high" (i.e., it is simultaneously less than cards of
 - value 2 and greater than cards of value King). -}
instance Ord Card where
  (<=) (Card Ace _) (Card Two _) = True
  (<=) (Card a _) (Card b _) = (<=) a b


-- The list of all possible suits
suits :: [Suit]
suits = [Spades, Hearts, Clubs, Diamonds]

-- The list of all possible values
values :: [Value]
values = [Two .. Ace]


-- Tries to interpret a human-readable shorthand for a card
card :: String -> Card
card str = let getValueFromChar '2' = Two
               getValueFromChar '3' = Three
               getValueFromChar '4' = Four
               getValueFromChar '5' = Five
               getValueFromChar '6' = Six
               getValueFromChar '7' = Seven
               getValueFromChar '8' = Eight
               getValueFromChar '9' = Nine
               getValueFromChar 'T' = Ten
               getValueFromChar 'J' = Jack
               getValueFromChar 'Q' = Queen
               getValueFromChar 'K' = King
               getValueFromChar 'A' = Ace
               getValueFromChar v   = error ("Can't parse value " ++ (show v))
               getSuitFromChar 'S' = Spades
               getSuitFromChar 'H' = Hearts
               getSuitFromChar 'C' = Clubs
               getSuitFromChar 'D' = Diamonds
               getSuitFromChar s   = error ("Can't parse suit " ++ (show s))
               val  = getValueFromChar (str !! 0)
               suit = getSuitFromChar (str !! 1)
           in Card val suit


-- The entire deck of 52 playing cards
deck :: [Card]
deck = [(Card value suit) | suit <- suits, value <- values]

-- All of the Kings in the deck
cowboys :: [Card]
cowboys = [card | card <- deck, (\(Card val _) -> (val == King)) card]

-- The face cards with only one eye showing
oneEyed :: [Card]
oneEyed = [card | card <- deck, (((Card King Diamonds) == card) || 
                                 ((Card Jack Spades) == card)   ||
                                 ((Card Jack Hearts) == card))]


-- Returns True if the card is an Ace
isAce :: Card -> Bool
isAce (Card val _) = val == Ace

-- Returns True if two cards are of the same suit
sameSuit :: Card -> Card -> Bool
sameSuit (Card _ suit1) (Card _ suit2) = suit1 == suit2

-- Returns True if every card in the list is of the same suit
sameSuits :: [Card] -> Bool
sameSuits [] = True
sameSuits (c:cs) = all (sameSuit c) cs

-- Returns True if two cards are of the same value
sameValue :: Card -> Card -> Bool
sameValue (Card val1 _) (Card val2 _) = val1 == val2

-- Returns True if every card in the list is of the same suit
sameValues :: [Card] -> Bool
sameValues [] = True
sameValues (c:cs) = all (sameValue c) cs

-- Returns True if the cards are adjacent and ascending (e.g., [2S, 3D, 4H])
ascending :: [Card] -> Bool
ascending ((Card val suit):cs) = av val cs
                           where av _ [] = True
                                 av val1 ((Card val2 _):xs)
                                   | (val1 == Ace) &&
                                     (val2 /= Two)       = False
                                   | (val1 == Ace) &&
                                     (val2 == Two)       = av val2 xs
                                   | (succ val1) == val2 = av val2 xs
                                   | otherwise           = False

-- Simulates a riffle shuffle
riffle :: ([Card], [Card]) -> [Card]
riffle (a, b) = collapse a b
                where collapse (a:as) (b:bs) = [a, b] ++ collapse as bs
                      collapse []     bs     = bs
                      collapse as     []     = as

-- The box shuffle
box :: [Card] -> [Card]
box deck = let a = splitAt 16 deck
               b = (splitAt 18 (snd a))
               c = snd b
           in c ++ (fst b) ++ (fst a)

-- Higher-order shuffle function that can perform multiple shuffles
shuffle :: [Card] -> ([Card] -> [Card]) -> Int -> [Card]
shuffle cs f i
  | i == 0    = cs
  | otherwise = shuffle (f cs) f (i - 1)

