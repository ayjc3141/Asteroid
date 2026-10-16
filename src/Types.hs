module Types where
import System.Random
import Graphics.Gloss.Data.Point

-- Addition and scalar multiplication between points/vectors
add :: Point -> Point -> Point
add (x0,y0) (x1,y1) = (x0+x1,y0+y1)

scale :: Float -> Point -> Point
scale t (x0,y0) = (t*x0, t*y0)

len :: Point -> Float
len (x,y) = sqrt (x*x + y*y)


data Input  = Input { turnLeft  :: Bool 
                    , turnRight  :: Bool
                    , thrust     :: Bool
                    , shoot      :: Bool 
                    } deriving (Show, Eq)

data EnemyType 
            = MeteorEnormous
            | AsteroidLarge
            | AsteroidMedium
            | AsteroidSmall
            | AsteroidMoving
            | SaucerEnormous
            | SaucerSmall
            deriving (Show, Eq)

data Movement = Movement { position :: Point
                            , velocity :: Point
                            , rotation :: Float 
                            } deriving (Show, Eq)

data Enemy = Enemy { enemyType      :: EnemyType
                    , enemyMovement  :: Movement 
                    , enemyHitpoints :: Int
                    , enemyAlive     :: Bool
                    , enemyHitbox    :: Float
                    } deriving (Show, Eq)

data Player = Player { playerNr            :: Int
                        , playerMovement   :: Movement
                        , playerAlive      :: Bool
                        , playerHitbox     :: Float 
                        , playerInput      :: Input
                        } deriving (Show, Eq)

data BulletType = Friend | Foe
                    deriving (Show, Eq)

data Bullet = Bullet { bulletMovement :: Movement
                        , bulletType     :: BulletType 
                        , bulletHitbox   :: Float
                        } deriving (Show,Eq)

data AnimationType = Death | Explosion deriving (Show, Eq)

data Animation = Animation { animationPosition  :: Point
                            , duration           :: Float
                            , animationType      :: AnimationType
                            } deriving (Show, Eq)

data GameState = GameState { players :: [Player]
                            , enemies :: [Enemy]
                            , bullets :: [Bullet]
                            , effects :: [Animation]
                            , score   :: Int
                            , lives   :: Int
                            , wave    :: Int
                            , paused  :: Bool
                            , rng     :: StdGen
                            } deriving (Show)

data HighScore = HighScore { playerName  :: String
                            , playerScore :: Int
                            } deriving (Show, Eq)

instance Ord HighScore where
    compare hs1 hs2 = compare (playerScore hs1) (playerScore hs2)

