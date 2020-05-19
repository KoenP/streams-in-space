module Game2 where

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
import Test.QuickCheck
import Linear.Epsilon (nearZero)

fps :: Num a => a
fps = 60

dt :: Fractional a => a
dt = 0.1 / fps

--------------------------------------------------------------------------------
-- PHYSICS
--------------------------------------------------------------------------------

integral :: VectorSpace v a => v -> Stream v -> Stream v
integral v0 v = transfer (^+^) v0 (dt *~ v)

type Pos          = Vec
type Force        = Vec
type Acceleration = Vec
type Velocity     = Vec
type Mass         = Float
type Length       = Float
type Angle        = Float

-- A Body has a position and a mass.
data Body = Body { bodyPos :: Pos, bodyMass :: Mass }
  deriving (Show, Eq)

-- (body |--) returns the gravitational field of body, that is, it
-- returns a function from positions in space to the force the gravity
-- of the body applies to a unit mass.
-- The field is ill-defined very close to the body.
(|--) :: Body -> (Pos -> Force)
Body pos1 mass |-- pos2 = (mass / dist^2) *^ diff
  where
    diff = pos1 ^-^ pos2
    dist = pos1 `distance` pos2

-- Gravity exerted by the first body on the second.
gravityOnSnd :: Body -> Body -> Force
gravityOnSnd b (Body pos mass) = mass *^ (b |-- pos)

calcBodyForces :: [Body] -> [Force]
calcBodyForces bodies
  = zipWith (^-^) (map sum tailGravities) (map sum $ nibble tailGravities)
  where
    -- Imagine bodies as the list [1,2,3]
    -- Then nonEmptyTails = [[1,2,3], [2,3], [3]]
    nonEmptyTails = P.takeWhile (not . null) . P.iterate tail $ bodies

    -- tailGravities = [[g12,g13], [g23], []],
    --   where gij = gravity applied on i by j
    tailGravities = map (\(p:ps) -> map (\p' -> gravityOnSnd p' p) ps) nonEmptyTails


    -- To compute the total gravity on each body, we want to get the list
    -- [(g12+g13) - 0, (g23) - (g12), 0 - (g13+g23)]
    -- Or, in other words, the difference of the lists
    -- [(g12+g13), g23         , 0         ] = map sum tailGravities
    -- [0        , g12         , g13+g23   ] = map sum (nibble tailGravities)

    -- Here nibble is a weird little function that, when given a list
    -- [[1,2,3], [4,5], [6]] as input, produces the list
    -- [[], [1], [2,4], [3,5,6]] as output.
    -- For performance, this implementation expects lists of
    -- monotonically increasing length without checking this
    -- condition, it just crashes if the shape isn't right.
    nibble = go 0
      where go n lists = case splitAt n lists of
              (h,[]) -> map head h : []
              (h, t) -> map head h : go (n+1) (map tail h ++ t)

--------------------------------------------------------------------------------
-- PLANETS
--------------------------------------------------------------------------------

-- The data associated with a planet that remains constant throughout
-- the simulation.
data PlanetInitialState
  = PlanetInitialState { planetColor    :: Color
                       , planetRadius   :: Length
                       , planetMass     :: Mass
                       , planetStartPos :: Pos
                       , planetStartVel :: Velocity
                       }
  deriving Show

initialStates :: [PlanetInitialState]
initialStates = sun

sun :: [PlanetInitialState]
sun = [ PlanetInitialState yellow 80 (rad2mass 80) (V2 0 (-200)) zeroV
      -- , PlanetInitialState blue 20 (rad2mass 20) (V2 (-300) 0) (V2 0 9000)
      ]
  where
    rad2mass r = 150 * (r ^ 3)
  
config8 :: [PlanetInitialState]
config8
  = [ PlanetInitialState yellow 15 (rad2mass 15) fig8Pos    ((-fig8Vel)/2)
    , PlanetInitialState green  15 (rad2mass 15) (-fig8Pos) ((-fig8Vel)/2)
    , PlanetInitialState blue   15 (rad2mass 15) zeroV      fig8Vel
    ]
  where
    fig8Pos = 600 *^ V2 0.97000436 (-0.24308753)
    fig8Vel = 600 *^ V2 (-0.93240737) (-0.86473146)
    rad2mass r = 900 * (r ^ 3)

config2 :: [PlanetInitialState]
config2 = [ PlanetInitialState blue  15 (rad2mass 15) (V2 (-200) 0) (V2 0 350)
          , PlanetInitialState green 15 (rad2mass 15) (V2   200  0) (V2 0 (-350))
          ]
  where
    rad2mass r = 800 * (r ^ 3)

config1 :: [PlanetInitialState]
config1 = [ PlanetInitialState blue  12 (rad2mass 12) (V2 (-200) 0) (V2 0 2500)
          , PlanetInitialState green 40 (rad2mass 40) (V2   200  0) (V2 0 (-250))
          ]
  where
    rad2mass r = 200 * (r ^ 3)

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

--------------------------------------------------------------------------------
-- GAME
--------------------------------------------------------------------------------
game :: Stream (Set Key) -> Stream Picture
game keys = centering $
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
      = liftA3 centerOn (fmap shipPos shipState) (fmap shipTheta shipState)

--------------------------------------------------------------------------------
-- TESTING
--------------------------------------------------------------------------------

instance Arbitrary Body where
  arbitrary = liftA2 Body arbitrary (fmap abs arbitrary)

prop_newtonWasRightAllAlong :: Body -> Body -> Property
prop_newtonWasRightAllAlong b1@(Body p1 _) b2@(Body p2 _)
  = p1 /= p2 ==> nearZero (gravityOnSnd b1 b2 ^+^ gravityOnSnd b2 b1)

prop_newtonWasRightAllAlong2 :: [Body] -> Bool
prop_newtonWasRightAllAlong2 bodies = nearZero $ sum (calcBodyForces bodies')
  where
    bodies' = nubBy ((==) `on` bodyPos) bodies
