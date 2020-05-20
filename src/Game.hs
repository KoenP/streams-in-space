module Game where

import Prelude hiding (scanl, iterate)
import qualified Prelude as P
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Key(..))
import Control.Applicative
import Data.Set (Set)
import Data.Maybe
import Data.List
import Data.Function
import qualified Data.Set as S
import Stream
import Vec
import Linear.Epsilon (nearZero)

import Physics
import Configs

fps :: Num a => a
fps = 60

dt :: Fractional a => a
dt = 0.1 / fps

integral :: VectorSpace v a => v -> Stream v -> Stream v
integral v0 v = transfer (^+^) v0 (dt *~ v)

--------------------------------------------------------------------------------
-- GAME
--------------------------------------------------------------------------------
game :: Stream (Set Key) -> Stream Picture
game keys = centering <*>
  merge [ renderedPlanets
        , planetTraces
        , fmap renderShip shipState
        , renderExhaust shipState
        , shipTrace
        ]
  where
    shipState = ship keys
    shipTrace = trace red (fmap shipPos shipState)
    planetTraces = merge $ zipWith trace planetColors planetPositions
    centering
      = centerOn <$> fmap shipPos shipState <*> fmap shipTheta shipState

--------------------------------------------------------------------------------
-- PLANETS
--------------------------------------------------------------------------------
initialStates :: [PlanetInitialState]
initialStates = config8

initBodies :: [Body]
initBodies = [ Body (planetStartPos s) (planetMass s) | s <- initialStates ]

planetMasses :: [Mass]
planetMasses = map planetMass initialStates

planetRadii :: [Length]
planetRadii = map planetRadius initialStates

planetColors :: [Color]
planetColors = map planetColor initialStates

planetPositions :: [Stream Pos]
planetPositions = zipWith3 simulateBody initBodies vel0s planetNetForces
  where
    vel0s  = map planetStartVel initialStates

planetBodies :: [Stream Body]
planetBodies = zipWith (liftA2 Body) planetPositions (map pure planetMasses)

planetNetForces :: [Stream Force]
planetNetForces = unseq
  $ fmap calcBodyForces (initBodies :. sequenceA planetBodies)
  where
    -- UNSAFELY assumes that the streams in the list have the same length.
    unseq :: Stream [a] -> [Stream a]
    unseq (l :. ls) = prepend l (unseq ls)
      where
        prepend (x:xs) s = (x :. head s) : prepend xs (tail s)
        prepend _      _ = []

simulateBody :: Body -> Velocity -> Stream Force -> Stream Pos
simulateBody (Body pos0 mass) vel0 netForce = integral pos0 velocity
  where
    velocity     = integral vel0 acceleration
    acceleration = netForce ~/ mass

type Field a = Pos -> a
gravitationalField :: Stream (Field Acceleration) -- force on unit mass
gravitationalField = (\fs -> sumV . sequenceA fs) <$> forceFields
  where
    forceFields :: Stream [Pos -> Force]
    forceFields = sequenceA $ map (fmap (|--)) planetBodies

--------------------------------------------------------------------------------
-- PLAYER
--------------------------------------------------------------------------------
data ShipState = ShipState { shipPos      :: Pos
                           , shipTheta    :: Angle
                           , shipThruster :: Bool
                           }
rotSpeed :: Num a => a
rotSpeed = 2000

initPos :: Pos
initPos = V2 0 400
  
-- Dimensionless value, determines the amount of thrust to produce.
thrustAmount :: Bool -> Float
thrustAmount True  = 10000
thrustAmount False = 0

ship :: Stream (Set Key) -> Stream ShipState
ship keys = liftA3 ShipState position theta thrusterOn
  where
    position        = integral initPos velocity
    velocity        = integral zeroV netAcceleration
    netAcceleration = thrust + (0.1 *~ gravity)
    gravity         = gravitationalField <*> (initPos :. position)
    theta           = integral 0 dTheta
      -- Current direction the ship is looking at.
      -- 0 degrees is straight up, increasing turns clockwise.

    thrust :: Stream Acceleration
    thrust          = (\th am -> th `rotVec` (am *^ upVec))
      <$> theta
      <*> fmap thrustAmount thrusterOn

    thrusterOn :: Stream Bool
    thrusterOn      = (Char 'w' `S.member`) <$> keys
    
    -- Amount of degrees the ship rotates in each frame (derivative of theta).
    dTheta          = calcDTheta <$> keys
      where
        calcDTheta ks = sum $ map snd $ filter ((`S.member` ks) . fst)
          [(Char 'a',(-rotSpeed)), (Char 'd',rotSpeed)]
  
--------------------------------------------------------------------------------
-- RENDERING
--------------------------------------------------------------------------------
isosceles :: Length -> Length -> Picture
isosceles w h = polygon [(0,h), ((-w/2),0), (w/2,0)]

renderShip :: ShipState -> Picture
renderShip (ShipState (V2 px py) theta thrusterOn) = color red
  $ translate px py
  $ rotate theta
  $ translate 0 (-4)
  $ isosceles 12 15

renderExhaust :: Stream ShipState -> Stream Picture
renderExhaust state
  = liftA2 suppressWhenFalse (fmap shipThruster state) (transformation <*> exhaust)
  where
    suppressWhenFalse :: Bool -> Picture -> Picture
    suppressWhenFalse True  pic = pic
    suppressWhenFalse False _   = blank

    transformation :: Stream (Picture -> Picture)
    transformation
      = (\(ShipState (V2 x y) theta _)
         -> translate x y . rotate (theta+180) . translate 0 4)
      <$> state

    exhaust = liftA2
      (\w h -> color yellow (isosceles w h))
      (slowCycle 2 [3,4,6])
      (slowCycle 2 [3,8,12])

    slowCycle n = Stream.cycle . concatMap (replicate n)
  
renderPlanet :: Length -> Color -> Pos -> Picture
renderPlanet rad col (V2 x y)
  = translate x y $ color col $ circleSolid rad

renderedPlanets :: Stream Picture
renderedPlanets
  = fmap pictures $ sequenceA $ zipWith fmap renderFromPos planetPositions
  where
    renderFromPos :: [Pos -> Picture]
    renderFromPos = zipWith renderPlanet planetRadii planetColors

trace :: Color -> Stream Pos -> Stream Picture
trace col pos = color col . line . map (\(V2 x y)->(x,y)) <$> history pos

merge :: [Stream Picture] -> Stream Picture
merge = fmap pictures . sequenceA

centerOn :: Pos -> Angle -> Picture -> Picture
centerOn (V2 x y) theta = rotate (-theta) . translate (-x) (-y) 
