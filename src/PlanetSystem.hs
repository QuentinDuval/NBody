module PlanetSystem where

import Constants
import Planet
import SpaceVect


-- | Initialization of the planet data

initPlanets :: [Planet]
initPlanets =
   let planets = [jupiter, saturn, uranus, neptune]
   in createSun planets : planets

createSun :: [Planet] -> Planet
createSun planets =
   let allMomentums = map computeMomentum planets
       computeMomentum p = multiplyConst (speed p) (mass p)
       sumMomentums = foldl plusVect nullVect allMomentums
   in Planet nullVect (multiplyConst sumMomentums (- 1/solarMass)) solarMass


-- | Data related to each planet

dp = daysPerYear

jupiter = Planet
   SpaceVect {x = 4.84143144246472090e+00, y = -1.16032004402742839e+00, z= -1.03622044471123109e-01}
   SpaceVect {x = 1.66007664274403694e-03*dp, y = 7.69901118419740425e-03*dp, z = -6.90460016972063023e-05*dp}
   (9.54791938424326609e-04 * solarMass)

saturn = Planet
   SpaceVect {x = 8.34336671824457987e+00, y = 4.12479856412430479e+00, z = -4.03523417114321381e-01}
   SpaceVect {x = -2.76742510726862411e-03*dp, y = 4.99852801234917238e-03*dp, z = 2.30417297573763929e-05*dp}
   (2.85885980666130812e-04 * solarMass)

uranus = Planet
   SpaceVect {x = 1.28943695621391310e+01,y = -1.51111514016986312e+01,z = -2.23307578892655734e-01}
   SpaceVect {x = 2.96460137564761618e-03*dp, y = 2.37847173959480950e-03*dp, z = -2.96589568540237556e-05*dp}
   (4.36624404335156298e-05 * solarMass)

neptune = Planet
   SpaceVect {x = 1.53796971148509165e+01, y = -2.59193146099879641e+01, z = 1.79258772950371181e-01}
   SpaceVect {x = 2.68067772490389322e-03*dp, y = 1.62824170038242295e-03*dp, z = -9.51592254519715870e-05*dp}
   (5.15138902046611451e-05 * solarMass)

