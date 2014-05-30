module PlanetTest where

import Planet
import SpaceVect


-- TODO - Put these tests in a difference executable in CABAL!


-- | Input data of the tests
{-
star =      Planet (SpaceVect 0 0 0) (SpaceVect 0 0 0) 10
planet1 =   Planet (SpaceVect 0 0 2) (SpaceVect 1 1 1) 1
planet2 =   Planet (SpaceVect 4 0 0) (SpaceVect 2 2 2) 2



-- | Run all tests

allTests :: IO()
allTests =
   let runTest (str, content) = putStrLn $ str ++ ": " ++ show content
   in mapM_ runTest [
         ("kineticEnergy", testKineticEnergy),
         ("potentialEnergy", testPotentialEnergy),
         ("systemEnergy", testEnergy),
         ("acceleration", testAcceleration)]


-- | Test: Computation of the total energy of the system

testKineticEnergy :: Bool
testKineticEnergy = res1 && res2 && res3 where
   res1 = kineticEnergy star == 0
   res2 = kineticEnergy planet1 == 3/2
   res3 = kineticEnergy planet2 == 12

testPotentialEnergy :: Bool
testPotentialEnergy = res1 && res2 && res3 where
   res1 = potentialEnergy star star == 0
   res2 = potentialEnergy star planet1 == -5
   res3 = potentialEnergy star planet2 == -5

testEnergy :: Bool
testEnergy =
   let expectedC = sum $ map kineticEnergy [star, planet1, planet2]
       expectedP = sum $ map (uncurry potentialEnergy)
                     [(star, planet1), (star, planet2), (planet1, planet2)]
   in energy [star, planet1, planet2] == expectedP + expectedC


-- | Test: Computation of the acceleration

testAcceleration :: Bool
testAcceleration = res1 && res2 where
   res1 = accelerationOn planet1 star == (SpaceVect 0 0 (-10/4))
   res2 = accelerationOn planet2 star == (SpaceVect (-5/8) 0 0)
-}

