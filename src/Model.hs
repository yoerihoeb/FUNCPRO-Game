-- | Game state and constants
module Model where

import System.Random (StdGen)

-- Geometry -----------------------------------------------------

type Vec2 = (Float, Float)

data Position = Position { px :: Float, py :: Float }
  deriving (Eq, Show)

addP :: Position -> Vec2 -> Position
addP (Position x y) (vx,vy) = Position (x+vx) (y+vy)

subP :: Position -> Position -> Vec2
subP (Position ax ay) (Position bx by) = (ax-bx, ay-by)

lenSq :: Vec2 -> Float
lenSq (x,y) = x*x + y*y

hitCircle :: Position -> Float -> Position -> Float -> Bool
hitCircle p1 r1 p2 r2 =
  let (dx,dy) = subP p1 p2
      rr = r1 + r2
  in dx*dx + dy*dy <= rr*rr

clamp :: Float -> Float -> Float -> Float
clamp lo hi v = max lo (min hi v)

-- Entities -----------------------------------------------------

data Player = Player
  { pPos      :: Position
  , pSpeed    :: Float
  , pCooldown :: Float
  } deriving (Show, Eq)

data EnemyType = Basic | Tracker | Shooter
  deriving (Show, Eq)

data Enemy = Enemy
  { ePos  :: Position
  , eVel  :: Vec2
  , eHP   :: Int
  , eType :: EnemyType
  } deriving (Show, Eq)

data Bullet = Bullet
  { bPos :: Position
  , bVel :: Vec2
  } deriving (Show, Eq)

data Anim =
    Explosion   { aPos :: Position, aTTL :: Float }
  | FlashHUD    { aTTL :: Float }
  | MuzzleFlash { aPos :: Position, aTTL :: Float }   -- NEW
  | BulletTrail { aPos :: Position, aTTL :: Float }   -- NEW
  deriving (Show, Eq)

-- Whole world --------------------------------------------------

data GameState = GameState
  { player        :: Player
  , enemies       :: [Enemy]
  , bullets       :: [Bullet]
  , anims         :: [Anim]
  , rng           :: StdGen
  , score         :: Int
  , health        :: Int
  , paused        :: Bool
  , timeAccum     :: Float
  , inputY        :: Float
  , highScorePath :: FilePath
  , gameOverSaved :: Bool
  , inputFire     :: Bool
  } deriving (Show)

-- Constants ----------------------------------------------------

screenW, screenH :: Float
screenW = 1024
screenH = 720

halfW, halfH :: Float
halfW = screenW/2
halfH = screenH/2

playerRadius, enemyRadius, bulletRadius :: Float
playerRadius = 20
enemyRadius  = 20
bulletRadius = 5

playerBaseSpeed, bulletSpeed, cooldownSecs :: Float
playerBaseSpeed = 320
bulletSpeed     = 560
cooldownSecs    = 0.18

enemyBaseSpeed :: Float
enemyBaseSpeed = 160

restartBtnW, restartBtnH, restartBtnY :: Float
restartBtnW = 240
restartBtnH = 64
restartBtnY = -120

-- Initial state ------------------------------------------------
initialGameState :: StdGen -> FilePath -> GameState
initialGameState gen hsPath = GameState
  { player = Player (Position (-halfW + 80) 0) playerBaseSpeed 0
  , enemies = []
  , bullets = []
  , anims = []
  , rng = gen
  , score = 0
  , health = 5
  , paused = False
  , timeAccum = 0
  , inputY = 0
  , highScorePath = hsPath
  , gameOverSaved = False
  , inputFire = False
  }

-- Public constructor used by Main
initialState :: StdGen -> GameState
initialState gen = initialGameState gen "highscores.txt"
