module PlayerMoves where
import Types
import Graphics.Gloss.Data.Point

-- rotates the player based on if the input of the player is true or false. currently only implemented for 1 player
rotatePlayer :: Float -> Player -> Player
rotatePlayer dt player              | l && not r = player   {playerMovement = 
                                                                (playerMovement player)
                                                                    {rotation = rotation (playerMovement player) - (240*dt)}}
                                    | r && not l = player   {playerMovement = 
                                                                (playerMovement player)
                                                                    {rotation = rotation (playerMovement player) + (240*dt)}}
                                    | otherwise = player
                                    where
                                        l = turnLeft (playerInput player)
                                        r = turnRight (playerInput player)

-- converts degrees to a vector of length 4
degToVec :: Float -> Point
degToVec t = ((cos rad), -(sin rad))
    where
        rad = t * pi / 180

--limits velocity of ship 
limitVelocity :: Point -> Point
limitVelocity vel | len vel > maxSpeed= (maxSpeed/ (len vel)) `scale` vel
                  | otherwise = vel
                  where
                    maxSpeed = 250

-- moves the player based on if 'w' is pressed or not
move :: Float -> Player -> Player
move dt player  | thrust (playerInput player) = player {playerMovement = 
                                                            (playerMovement player)
                                                                {velocity = newVel , position = newPos}}
                | otherwise = player {playerMovement = 
                                        (playerMovement player)
                                            {position = newPos}}
                where
                    vel = velocity (playerMovement player)
                    pos = position (playerMovement player)
                    angle = rotation (playerMovement player)
                    newVel = limitVelocity (vel `add` ((dt*100) `scale` (degToVec angle)))
                    newPos = pos `add` (dt `scale` vel)
        

