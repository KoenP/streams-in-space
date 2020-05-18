module Game where

import Prelude hiding (scanl, iterate)
import qualified Prelude as P
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Key(..))
import Control.Applicative
import Data.Set (Set)
import Data.Maybe
import qualified Data.Set as S
import Stream
import Vec
import Debug.Trace

fps :: Num a => a
fps = 60

dt :: Fractional a => a
dt = 0.2 / fps

integral :: VectorSpace v a => v -> Stream v -> Stream v
integral v0 v = transfer (^+^) v0 (fmap (dt*^) v)

keyPressed :: Key -> Stream (Set Key) -> Stream Bool
keyPressed key = fmap (S.member key)

startPos :: Vec
startPos = V2 0 400

spaceship :: Stream Float -> Stream Float -> Stream (Vec,Float)
spaceship acceleration rotation = liftA2 (,) position theta
  where
    position        = integral startPos velocity
    velocity        = integral zeroV netAcceleration
    netAcceleration = (^+^)
      <$> liftA2 (*^) acceleration thrust
      <*> gravity
    gravity         = fmap (0.006 *^) (gravitationalField <*> (startPos :. position))
    thrust          = fmap (\t -> t `rotVec` upVec) theta
    theta           = integral 0 rotation

isosceles :: Float -> Float -> Picture
isosceles w h = polygon [(0,h), ((-w/2),0), (w/2,0)]

thruster :: Stream Bool -> Stream (Vec,Float) -> Stream Picture
thruster accelerating posTheta
  = liftA2 suppressWhenFalse accelerating (transformation <*> thrust)
  where
    suppressWhenFalse :: Bool -> Picture -> Picture
    suppressWhenFalse True  pic = pic
    suppressWhenFalse False _   = blank
    
    transformation :: Stream (Picture -> Picture)
    transformation
      = (\(V2 x y,theta) -> translate x y . rotate (theta+180) . translate 0 4)
      <$> posTheta

    thrust = liftA2
      (\w h -> color yellow (isosceles w h))
      (slowCycle 2 [3,4,6])
      (slowCycle 2 [3,8,12])

    slowCycle n = Stream.cycle . concatMap (replicate n)

renderShip :: (Vec,Float) -> Picture
renderShip (V2 px py,angle) = color red
  $ translate px py
  $ rotate angle
  $ translate 0 (-4)
  $ isosceles 12 15
  -- $ polygon [(0,12), ((-6),(-3)), (5,(-3))]

rotSpeed :: Num a => a
rotSpeed = 500

rotation :: Stream (Set Key) -> Stream Float
rotation = fmap $ \keysPressed -> head $
  [ speed
  | (key,speed) <- [(Char 'a', -rotSpeed), (Char 'd', rotSpeed)]
  , key `S.member` keysPressed
  ] ++ [0]

acceleration :: Stream Bool -> Stream Float
acceleration = fmap $ \case
  True  -> 1200
  False -> 0
  -- if   Char 'w' `S.member` keysPressed 
  -- then 200
  -- else 0

accelerating :: Stream (Set Key) -> Stream Bool
accelerating = fmap $ S.member (Char 'w')

game :: Stream (Set Key) -> Stream Picture
game keysPressed
  = centerShip
  <*> ((\ps ts ss st fl -> pictures [st,ts,ps,ss,fl])
  <$> renderedPlanets
  <*> renderedTraces
  <*> renderedShip
  <*> shipTrace
  <*> flame)
  where
    renderedPlanets = pictures . map renderPlanet <$> sequenceA planets
    renderedTraces  = fmap pictures . sequenceA
      $ map applyRenderHistory
      $ planetColors `zip` map (history . fmap planetPos) planets
    renderedShip    = fmap renderShip ship

    planetColors    = map (planetColor . fst) planetInits

    applyRenderHistory :: (Color, Stream [Vec]) -> Stream Picture
    applyRenderHistory (col,h) = renderHistory col <$> h

    renderHistory :: Color -> [Vec] -> Picture
    renderHistory col = color col . line . map (\(V2 x y) -> (x,y))

    -- TODO naming
    accelerating' = accelerating keysPressed
    acceleration' = acceleration accelerating'

    ship = spaceship acceleration' (rotation keysPressed)

    shipTrace = applyRenderHistory (red, history (fmap fst ship))

    flame = thruster accelerating' ship

    centerShip =
      (\(V2 x y,theta) -> rotate (-theta) . translate (-x) (-y))
      <$> ship

data Planet = Planet
  { planetPos    :: Vec
  , planetRadius :: Float
  , planetColor  :: Color
  }
  deriving (Show)

--       m1*m2
-- F = G -----
--        r^2 

-- Ill-defined when r very small.
-- Returns the force of gravity applied on planet 1 by planet 2.
gravity :: Planet -> Planet -> Vec
gravity (Planet p1 r1 _) (Planet p2 r2 _) = (m1 * m2 / r^2) *^ vec
  where
    vec = normalize (p2 ^-^ p1)
    m1  = rad2mass r1
    m2  = rad2mass r2
    r   = p1 `distance` p2

rad2mass :: Float -> Float
rad2mass r = 2000 * (r ^ 3)


calcPlanetGravities :: [Planet] -> [Vec]
calcPlanetGravities planets
  = zipWith (^-^) (map sum tailGravities) (map sum $ nibble tailGravities)
  where
    -- Imagine planets as the list [1,2,3]
    -- Then nonEmptyTails = [[1,2,3], [2,3], [3]]
    nonEmptyTails = P.takeWhile (not . null) . P.iterate tail $ planets

    -- tailGravities = [[g12,g13], [g23], []],
    --   where gij = gravity applied on i by j
    tailGravities = map (\(p:ps) -> map (gravity p) ps) nonEmptyTails

    -- To compute the total gravity on each planet, we want to get the list
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



planetInits :: [(Planet,Vec)]
planetInits = onePlanet

onePlanet :: [(Planet,Vec)]
onePlanet = [ (Planet zeroV 40 blue, zeroV) ]

experiment :: [(Planet,Vec)]
experiment = [ (Planet (V2 (-200) 0) 12 green, V2 0 500)
             , (Planet (V2   200  0) 40 blue  , zeroV)
             ]

experiment2 :: [(Planet,Vec)]
experiment2 = [ (Planet (V2 (-200) 0) 30 green, V2 0 250)
              , (Planet (V2   200  0) 30 blue , V2 0 (-250))
              ]

fig8 :: [(Planet,Vec)]
fig8 = [ (Planet fig8Pos    15 yellow , (-fig8Vel) / 2)
       , (Planet (-fig8Pos) 15 green  , (-fig8Vel) / 2)
       , (Planet zeroV      15 blue   , fig8Vel)
       ]
  where
    fig8Pos = 200 *^ V2 0.97000436 (-0.24308753)
    fig8Vel = 200 *^ V2 (-0.93240737) (-0.86473146)

gravitationalField :: Stream (Vec -> Vec)
gravitationalField
  = (\ps v -> sumV $ map (force v) ps) <$> sequenceA planets
  where
    force pos1 (Planet pos2 rad _)
      = let m   = rad2mass rad
            vec = pos2 ^-^ pos1
            r   = norm vec
        in (m / r^2) *^ vec

planets :: [Stream Planet]
planets = zipWith simulatePlanet planetInits gravities

gravities :: [Stream Vec]
gravities = unseq
    $ fmap calcPlanetGravities (map fst planetInits :. sequenceA planets)
  where
    -- UNSAFELY assumes that the streams in the list have the same length.
    unseq :: Stream [a] -> [Stream a]
    unseq (l :. ls) = prepend l (unseq ls)
      where
        prepend (x:xs) s = (x :. head s) : prepend xs (tail s)
        prepend _      _ = []

simulatePlanet :: (Planet,Vec) -> Stream Vec -> Stream Planet
simulatePlanet (Planet pos0 rad col, impulse) netForce
  = liftA3 Planet position (pure rad) (pure col)
  where
    position     = integral pos0 velocity
    velocity     = integral impulse acceleration
    acceleration = (^/ rad2mass rad) <$> netForce 

-- planet1Init :: (Planet,Vec)
-- planet1Init = (Planet (V2 (-200) 0) 12 red  , V2 0 50)
-- 
-- planet2Init :: (Planet,Vec)
-- planet2Init = (Planet (V2   200  0) 90 blue , zeroV)

-- planet1 :: Stream Planet
-- planet1 = liftA3 Planet pos (pure r) (pure col)
--   where
--     pos = pos0 :. integral pos0 v
--     v   = integral impulse a
--     a   = (\f' -> f' ^/ m) <$> f
--     f   = liftA2 gravity planet1 planet2
-- 
--     ((Planet pos0 r col), impulse) = planet1Init
--     m = rad2mass r
-- 
-- planet2 :: Stream Planet
-- planet2 = liftA3 Planet pos (pure r) (pure col)
--   where
--     pos = pos0 :. integral pos0 v
--     v   = integral impulse a
--     a   = (\f' -> f' ^/ m) <$> f
--     f   = liftA2 gravity planet2 planet1
-- 
--     ((Planet pos0 r col), impulse) = planet2Init
--     m = rad2mass r


renderPlanet :: Planet -> Picture
renderPlanet (Planet (V2 x y) radius col)
  = translate x y $ color col $ circleSolid radius

-- initState :: [(Planet,Vec)]
-- initState
--   = [ (Planet (V2 (-200) 0) 50 red , zeroV)
--     , (Planet (V2 200    0) 75 blue, zeroV)
--     ]

-- simulatePlanet :: Stream [Planet] -> Stream Planet
-- simulatePlanet 
-- 
-- simulatePlanets :: Stream [Planet]
-- simulatePlanets 
