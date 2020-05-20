module Configs where

import Graphics.Gloss
import Physics
import Vec

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
