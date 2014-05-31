module Main (main) where

import Control.Monad
import Planet
import PlanetSystem
import System.Environment
import Text.Printf


main::IO()
main = do 
   let deltaTime = 0.01 :: Double
   n <- getArgs >>= readIO.head
   planets <- initPlanets
   printf "%.9f\n" =<< energy planets
   replicateM_ n (advance deltaTime planets)
   printf "%.9f\n" =<< energy planets

