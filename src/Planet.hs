module Planet where

import Control.DeepSeq
import SpaceVect
import Utils


-- | Planet data
data Planet = Planet {
   position :: {-# UNPACK #-} !SpaceVect,
   speed    :: {-# UNPACK #-} !SpaceVect,
   mass     :: {-# UNPACK #-} !Double }
   deriving (Show);

instance NFData Planet where
   rnf (Planet p s m) = p `deepseq` s `deepseq` m `deepseq` ()


-- | Computation of the total energy of the system
energy :: [Planet] -> Double
energy planets =
   let allKinetic = sum $ map kineticEnergy planets
       allPotential = sum $ map (uncurry potentialEnergy) (combinations planets)
   in force $ allKinetic + allPotential


-- | Kinetic energy = 1/2 * m * v^2
kineticEnergy :: Planet -> Double
kineticEnergy p = 0.5 * mass p * normSquared (speed p) 


-- | Potential energy = - G * m1 * m2 / r
potentialEnergy :: Planet -> Planet -> Double
potentialEnergy p1 p2 =
   let distance = sqrt $ distanceSquared (position p1) (position p2)
   in if distance > 0
      then force $ - mass p1 * mass p2 / distance
      else 0


-- | Advance all planets, one step, based on delta T
advance :: Double -> [Planet] -> [Planet]
advance dt planets =
   let newSpeeds = interactAll (nextSpeed dt) planets
   in zipWith (nextPosition dt) planets newSpeeds


-- | Compute the next position of a planet
nextPosition :: Double -> Planet -> SpaceVect -> Planet
nextPosition dt !p newSpeed =
   let newPosition = position p `plusVect` multiplyConst newSpeed dt
   in p { position = newPosition, speed = newSpeed }


-- | Advance planet, one step, based on delta T
nextSpeed :: Double -> Planet -> [Planet] -> SpaceVect
nextSpeed dt p otherPlanets =
   let accelarations = map (accelerationOn p) otherPlanets
       totalAcc = foldl plusVect nullVect accelarations
   in speed p `plusVect` multiplyConst totalAcc dt


-- | Compute the acceleration of a planet on the other
-- F12 = -G * (m1 * m2 / r^2) * u12 - divided by m1 to get the acceleration
accelerationOn :: Planet -> Planet -> SpaceVect
accelerationOn p1 p2 =
   let distVector = position p1 `minusVect` position p2
       distSquared = normSquared distVector
       factor = - mass p2 / (distSquared * sqrt distSquared)
   in multiplyConst distVector factor

