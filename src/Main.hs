module Main (main) where

import Planet
import PlanetSystem
import Text.Printf


main::IO()
main =
   let deltaTime = 0.01 :: Double
       planets = initPlanets
       planets' = iterateN 500000 (advanceAll deltaTime) planets
   in do
      printf "%.9f\n" $ energy planets
      printf "%.9f\n" $ energy planets'


iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 _ !i = i
iterateN !n !f !i = iterateN (n-1) f (f i)

-- Does the job, just leads to stack overflow
--iterateN n f = last . take (n + 1) . iterate f
