-- Main.hs

import Poker
import Card
import Shuffle
import Show

main = do
    washed <- wash deck
    let shuffled = shuffle washed ((riffle.(splitAt 26)).box
                                    .(riffle.(splitAt 26))
                                    .(riffle.(splitAt 26))) 1
        (remaining, pockets) = deal shuffled 2 3
        yours = pockets !! 0

    putStrLn $ "Hole cards:\t" ++ show yours ++ "\t\t" ++ show (hand yours)

    let (rest, flop, turn, river) = dealBoard remaining
    putStrLn $ "The flop:\t" ++ show flop
               ++ "\t" ++ show (hand (yours ++ flop))
    putStrLn $ "The turn:\t" ++ show turn
               ++ "\t\t" ++ show (hand (yours ++ flop ++ [turn]))
    putStrLn $ "The river:\t" ++ show river
               ++ "\t\t" ++ show (hand (yours ++ flop ++ [turn] ++ [river]))
    putStrLn ""
