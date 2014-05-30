module Main (main) where

import Planet
import PlanetSystem
import System.Environment
import Text.Printf


main::IO()
main =
   let deltaTime = 0.01 :: Double
       planets = initPlanets
   in do
      n <- getArgs >>= readIO.head
      printf "%.9f\n" $ energy planets
      let planets' = iterateN n (advance deltaTime) planets
      printf "%.9f\n" $ energy planets'


iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 _ !i = i
iterateN !n !f !i = iterateN (n-1) f (f i)

-- Does the job, just leads to stack overflow
--iterateN n f = last . take (n + 1) . iterate f
