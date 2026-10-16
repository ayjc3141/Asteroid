module Collision where

import Graphics.Gloss.Data.Point

collide :: Point -> Float -> Point -> Float -> Bool -- Checks whether 2 objects (Bullets, enemies or players) collide
collide (x0,y0) s (x1,y1) t = dis (x0,y0) (x1,y1)  <= s + t 
    where
        dis (x0,y0) (x1,y1) = sqrt ((x0-x1)^2 + (y0-y1)^2)
