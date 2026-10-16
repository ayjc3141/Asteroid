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
degToVec t = (4*(cos rad), -4*(sin rad))
    where
        rad = t * pi / 180

-- moves the player based on if 'w' is pressed or not
move :: Player -> Player
move player | thrust (playerInput player) = player {playerMovement = 
                                                            (playerMovement player)
                                                                {velocity = degToVec (rotation (playerMovement player))
                                                                ,position = (position (playerMovement player)) `add` (degToVec (rotation (playerMovement player)))}
                                                    }
            | otherwise = player    {playerMovement = 
                                        (playerMovement player)
                                            {velocity = (0,0)}
                                    }

