-- Main.hs

import Poker
import Card
import Shuffle
-- import Stats
import Maybe
import List

main = do
    washed <- wash deck

    -- Riffle, riffle, box, riffle
    let shuffled = shuffle washed ((riffle.(divide 26)).box.(riffle.(divide 26)).(riffle.(divide 26))) 1
        (remaining, pockets) = deal shuffled 2 3
        yours = pockets !! 0

    putStrLn $ "Hole cards:\t" ++ show yours ++ "\t\t" ++ show ((fromJust.hand) yours)

    let (rest, flop, turn, river) = dealBoard remaining
    putStrLn $ "The flop:\t" ++ show flop ++ "\t" ++ show (multiHand (yours ++ flop))
    putStrLn $ "The turn:\t" ++ show turn ++ "\t\t" ++ show (multiHand (yours ++ flop ++ [turn]))
    putStrLn $ "The river:\t" ++ show river ++ "\t\t" ++ show (multiHand (yours ++ flop ++ [turn] ++ [river]))
    putStrLn ""
