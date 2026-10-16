module Main (main) where
import Graphics.Gloss.Interface.IO.Game
import Types 
import Collision 
import Input
import PlayerMoves
import Graphics.Gloss.Data.Point
main :: IO ()
main = playIO  
                (InWindow "Asteroid" (800,800) (100,100)) 
                (makeColor 0 0 0 1)
                60
                (Player 1 (Movement (0,0) (0,0) 0) True 1.0 (Input False False False False))
                viewPlayer
                (\event player -> pure (takeInput event player))
                updatePlayer

updatePlayer :: Float -> Player -> IO Player
updatePlayer dt player = pure (rotatePlayer dt (move dt player))

viewPlayer :: Player -> IO Picture
viewPlayer player = pure
                        (translate x0 y0
                            (color white
                                (rotate angle
                                    (Polygon [(25,0), (-25,-20), (-25,20)])
                                )
                            )
                        )
                    where
                        (x0,y0) = position (playerMovement player)
                        angle = rotation (playerMovement player)
    
                    