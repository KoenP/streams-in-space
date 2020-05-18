module Main where

--------------------------------------------------------------------------------
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as S
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Stream
import Game
--------------------------------------------------------------------------------

main :: IO ()
main = run game

-- seeyou :: Stream (Set Key) -> Stream Picture
-- seeyou = fmap render . transfer update 0 . keyPressed (Char 'w')
--   where
--     update pos True  = pos + 10
--     update pos False = pos
-- 
--     render pos = translate (-100) pos . scale 0.2 0.2 . color white $ text "See you Wednesday!"

run :: (Stream (Set Key) -> Stream Picture) -> IO ()
run sf = do
  keysPressed <- newIORef S.empty
  inputs      <- inputStream (readIORef keysPressed)
  outputs     <- newIORef (sf inputs)

  let
    handleEvent (EventKey k Down _ _) _ =
      keysPressed `modifyIORef` S.insert k
    handleEvent (EventKey k Up   _ _) _ =
      keysPressed `modifyIORef` S.delete k
    handleEvent _                       _ =
      return ()

  let
    render _ = do
      (pic :. pics) <- readIORef outputs
      outputs `writeIORef` pics
      return pic
  
  playIO
    (InWindow "Streams in Spaaaaaaaace!" (1024,960) (200,200))
    black
    fps
    ()
    render
    handleEvent
    (\_ _ -> return ())
