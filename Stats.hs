-- Stats.hs

module Stats
  (choose)
where

-- Factorial
fact :: Integer -> Integer
fact 0 = 1
fact 1 = 1
fact n = n * fact (n - 1)

-- Binomial coefficient
choose :: Integer -> Integer -> Integer
choose 0 0 = 0
choose n k
  | k < 0     = error "k must be greater than or equal to 0"
  | n < k     = error "n must be greater than or equal to k"
  | otherwise = (fact n) `div` ((fact k) * (fact (n - k)))

{- The probability of a given hand is calculated by dividing the number
 - of ways of drawing the hand by the total number of 5-card hands (the
 - sample space, u, which is equal to (choose 52 5)). -}
u :: Integer
u = 52 `choose` 5

probability ways = (toRational ways) / (toRational u)
