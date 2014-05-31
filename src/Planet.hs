module Planet where

import Control.DeepSeq
import Control.Monad
import Data.IORef
import Data.Vector as V
import SpaceVect


-- | Planet data
data Planet = Planet {
   position :: {-# UNPACK #-} !(IORef SpaceVect),
   speed    :: {-# UNPACK #-} !(IORef SpaceVect),
   mass     :: {-# UNPACK #-} !Double }


-- | Computation of the total energy of the system
energy :: Vector Planet -> IO Double
energy planets = do
   energies <- Prelude.mapM (energyPlanet planets) [0 .. V.length planets - 1]
   return $!! Prelude.sum energies


-- | Computation of the energy of a planet in the system
energyPlanet :: Vector Planet -> Int -> IO Double
energyPlanet planets i = do
   kineticE <- kineticEnergy $ planets ! i
   potentialE <- potentialEnergy planets i
   return $!! kineticE + potentialE


-- | Kinetic energy = 1/2 * m * v^2
kineticEnergy :: Planet -> IO Double
kineticEnergy !p = do
   s <- readIORef (speed p)
   return $!! 0.5 * mass p * normSquared s


-- | Potential energy of the planet at index i
potentialEnergy :: Vector Planet -> Int -> IO Double
potentialEnergy planets i = do
   let ps = V.drop (i+1) planets
   e <- V.mapM (potentialEnergy' $ planets V.! i) ps
   return $!! V.sum e


-- | Potential energy between two planets = - G * m1 * m2 / r
potentialEnergy' :: Planet -> Planet -> IO Double
potentialEnergy' !p1 !p2 = do
   [pos1, pos2] <- Prelude.mapM (readIORef . position) [p1, p2]
   let distance = sqrt $ distanceSquared pos1 pos2
   return $!!
      if pos1 == pos2 then 0
      else - mass p1 * mass p2 / distance


-- | Advance all planets, one step, based on delta T
advance :: Double -> Vector Planet -> IO ()
advance dt planets = do
   newSpeeds <- Prelude.mapM (nextSpeed dt planets) [0 .. V.length planets - 1]
   V.zipWithM_ (nextPosition dt) planets (fromList newSpeeds)


-- | Compute the next position of a planet
nextPosition :: Double -> Planet -> SpaceVect -> IO ()
nextPosition dt !p !newSpeed = do
   modifyIORef' (position p) (`plusVect` multiplyConst newSpeed dt)
   writeIORef (speed p) $!! newSpeed


-- | Advance planet, one step, based on delta T
nextSpeed :: Double -> Vector Planet -> Int -> IO SpaceVect
nextSpeed dt !planets i = do
   let p = planets ! i
       acc j p' = if j == i then return nullVect else accelerationOn p p'
   accelarations <- V.mapM (uncurry acc) (indexed planets)
   let totalAcc = V.foldl plusVect nullVect accelarations
   s <- readIORef (speed p)
   return $!! s `plusVect` multiplyConst totalAcc dt


-- | Compute the acceleration of a planet on the other
-- F12 = -G * (m1 * m2 / r^2) * u12 - divided by m1 to get the acceleration
accelerationOn :: Planet -> Planet -> IO SpaceVect
accelerationOn p1 p2 = do
   [pos1, pos2] <- Prelude.mapM (readIORef . position) [p1, p2]
   let distVector = pos1 `minusVect` pos2
       distSquared = normSquared distVector
       factor = - mass p2 / (distSquared * sqrt distSquared)
   return $!! multiplyConst distVector factor

