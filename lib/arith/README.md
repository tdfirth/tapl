# Arith

The first language from TAPL, it doesn't really do much other than represent numbers in a Peano arithmetic sort of way. Still, it's a good example for laying the foundations of more complex languages to come.

I have implemented two evaluators for it, one in the small step style which is given in the reference implementation by Pierce. The other in the big step style which was left as an exercise to the reader.

The module `arith.ml` just exposes a functor for constructing the language and the two evaluator implementations, leaving it up to the user to decide which they want to use.
