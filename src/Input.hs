module Input where
import Types
import PlayerMoves
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Point

takeInput :: Event -> Player -> Player
takeInput event player =
    case event of 
        (EventKey (Char 'w') Down _ _) -> player {playerInput = press {thrust = True}}
        
        (EventKey (Char 'w') Up _ _) -> player {playerInput = press {thrust = False}}
        
        (EventKey (Char 'a') Down _ _) -> player {playerInput = press {turnLeft = True}}

        (EventKey (Char 'd') Down _ _) -> player {playerInput = press {turnRight = True}}
                
        (EventKey (Char 'a') Up _ _) -> player {playerInput = press {turnLeft = False}}

        (EventKey (Char 'd') Up _ _) -> player {playerInput = press {turnRight = False}}
        _ -> player 
        where
            press = playerInput player


