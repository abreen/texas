texas - A poker experiment in Haskell
=====================================

`texas` is a collection of Haskell scripts I have played around with in my free time. The package currently contains a couple of (mostly incomplete) modules related to playing cards and the game of poker.

The `Card` module contains a representation of suits, values, and cards. It contains instantiations of `Ord` on values and cards (though this probably could be more elegant). The module also contains functions to help convert strings to `Card`s, binary comparison functions and comparison functions on lists, and functions for dividing and shuffling cards. There are also some useful values, namely: `suits`, the list of all suits in a standard deck; `values`, the list of all possible values (or ranks) of cards; and `deck`, the list of all 52 playing cards. Novelty values are also included (see `cowboys` and `oneEyed`).

The `Shuffle` module contains the `wash` function, a shameless adaptation of the [function from the Haskell wiki][rnd].

The `Poker` module contains the `Hand` type, a representation of possible poker hands. It also contains the `hand` function, which will returns the most valuable hand represented by a list of cards. In this module are functions that do poker deals, including a function `dealBoard` that simulates a Texas hold 'em deal.

The `Stats` module is mostly empty now, but it should eventually contain some tools for calculating poker hand probability.

The `Main` module is a small example of how the scripts can work together. It simulates one round of a Texas hold 'em game with three players, without betting and without determining a winner.

If you find that some of the code would be useful in your project, you may use it, but you should mention me and perhaps this repository.

[rnd]: http://www.haskell.org/haskellwiki/Random_shuffle
