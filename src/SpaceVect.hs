module SpaceVect where

import Control.DeepSeq


-- | Strictly evaluated space vector
data SpaceVect = SpaceVect {
   x :: {-# UNPACK #-} !Double,
   y :: {-# UNPACK #-} !Double,
   z :: {-# UNPACK #-} !Double
} deriving (Show, Eq);


instance NFData SpaceVect where
   rnf (SpaceVect x y z) = x `deepseq` y `deepseq` z `deepseq` ()


-- | Utils functions to manipulate space vectors

nullVect :: SpaceVect
nullVect = SpaceVect 0 0 0

multiplyConst :: SpaceVect -> Double -> SpaceVect
multiplyConst (SpaceVect x y z) !f = SpaceVect (x*f) (y*f) (z*f)

multiplyVect :: SpaceVect -> SpaceVect -> SpaceVect
multiplyVect (SpaceVect x y z) (SpaceVect x' y' z') =
   SpaceVect (x*x') (y*y') (z*z')

plusVect :: SpaceVect -> SpaceVect -> SpaceVect
plusVect (SpaceVect x y z) (SpaceVect x' y' z') =
   SpaceVect (x+x') (y+y') (z+z')

minusVect :: SpaceVect -> SpaceVect -> SpaceVect
minusVect (SpaceVect x y z) (SpaceVect x' y' z') =
   SpaceVect (x-x') (y-y') (z-z')

normSquared :: SpaceVect -> Double
normSquared (SpaceVect x y z) = x^2 + y^2 + z^2

distanceSquared :: SpaceVect -> SpaceVect -> Double
distanceSquared p1 p2 = normSquared (minusVect p1 p2)


