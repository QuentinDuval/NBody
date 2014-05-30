NBody
=====

A simple implementation of the NBody system problem available in the "Computer Language Benchmarks Game".
Based on immutable objects, this implementation:
- only makes use of pure functions (no ST nor IO monad)
- does not use the foreign pointer package 

It ends up being around 2 to 2.5 times slower than the implementations based on mutable data:
http://benchmarksgame.alioth.debian.org/u32/program.php?test=nbody&lang=ghc&id=2

I believe it to be closer to the kind of programs you would generally observe in Haskell.
The idea was to get a general idea regarding how pure Haskell programs perform against other languages.

