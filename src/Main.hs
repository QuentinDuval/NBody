module Main (main) where

import Control.Monad
import Planet
import PlanetSystem
import System.Environment
import Text.Printf


main::IO()
main =
   let deltaTime = 0.01 :: Double
   in do
      n <- getArgs >>= readIO.head
      planets <- initPlanets
      printf "%.9f\n" =<< energy planets
      replicateM_ n (advance deltaTime planets)
      printf "%.9f\n" =<< energy planets

