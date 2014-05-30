module Planet where

import Control.DeepSeq
import Control.Monad
import Data.IORef

import SpaceVect
import Utils


-- | Planet data
data Planet = Planet {
   position :: {-# UNPACK #-} !(IORef SpaceVect),
   speed    :: {-# UNPACK #-} !(IORef SpaceVect),
   mass     :: {-# UNPACK #-} !Double }


-- | Computation of the total energy of the system
energy :: [Planet] -> IO Double
energy planets = do
   allKinetic <- mapM kineticEnergy planets
   allPotential <- mapM (uncurry potentialEnergy) (combinations planets)
   return $!! sum allKinetic + sum allPotential


-- | Kinetic energy = 1/2 * m * v^2
kineticEnergy :: Planet -> IO Double
kineticEnergy !p = do
   s <- readIORef (speed p)
   return $!! 0.5 * mass p * normSquared s


-- | Potential energy = - G * m1 * m2 / r
potentialEnergy :: Planet -> Planet -> IO Double
potentialEnergy !p1 !p2 = do
   [pos1, pos2] <- mapM (readIORef . position) [p1, p2]
   let distance = sqrt $ distanceSquared pos1 pos2
   return $!!
      if pos1 == pos2 then 0
      else force $ - mass p1 * mass p2 / distance


-- | Advance all planets, one step, based on delta T
advance :: Double -> [Planet] -> IO ()
advance dt planets = do
   newSpeeds <- sequence $ interactAll (nextSpeed dt) planets
   zipWithM_ (nextPosition dt) planets newSpeeds


-- | Compute the next position of a planet
nextPosition :: Double -> Planet -> SpaceVect -> IO ()
nextPosition dt !p !newSpeed = do
   pos <- readIORef (position p)
   writeIORef (position p) $!! pos `plusVect` multiplyConst newSpeed dt
   writeIORef (speed p)    $!! newSpeed


-- | Advance planet, one step, based on delta T
nextSpeed :: Double -> Planet -> [Planet] -> IO SpaceVect
nextSpeed dt !p !otherPlanets = do
   accelarations <- mapM (accelerationOn p) otherPlanets
   let totalAcc = foldl plusVect nullVect accelarations
   s <- readIORef (speed p)
   return $!! s `plusVect` multiplyConst totalAcc dt


-- | Compute the acceleration of a planet on the other
-- F12 = -G * (m1 * m2 / r^2) * u12 - divided by m1 to get the acceleration
accelerationOn :: Planet -> Planet -> IO SpaceVect
accelerationOn p1 p2 = do
   [pos1, pos2] <- mapM (readIORef . position) [p1, p2]
   let distVector = pos1 `minusVect` pos2
       distSquared = normSquared distVector
       factor = - mass p2 / (distSquared * sqrt distSquared)
   return $!! multiplyConst distVector factor

