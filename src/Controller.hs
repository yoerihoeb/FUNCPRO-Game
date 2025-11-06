-- | Time step and input handling, plus core logic
module Controller where

import Graphics.Gloss.Interface.IO.Game
import System.Random (StdGen, randomR)
import qualified System.IO as IO

import Model

-- External API required by the framework ----------------------

step :: Float -> GameState -> IO GameState
step dt gs
  | health gs <= 0 =
      if gameOverSaved gs
        then pure (gs { anims = tickAnims dt (anims gs) })
        else do IO.appendFile (highScorePath gs) (show (score gs) ++ "\n")
                pure gs { gameOverSaved = True, paused = True }
  | paused gs = pure (gs { anims = tickAnims dt (anims gs) })
  | otherwise = pure (stepWorld dt gs)

input :: Event -> GameState -> IO GameState
input ev gs = pure (inputKey ev gs)

-- Helpers ------------------------------------------------------

resetGame :: GameState -> GameState
resetGame gs = initialGameState (rng gs) (highScorePath gs)

pointInRect :: (Float, Float) -> (Float, Float, Float, Float) -> Bool
pointInRect (mx,my) (cx,cy,w,h) =
  let halfW' = w/2; halfH' = h/2
  in mx >= cx - halfW' && mx <= cx + halfW' && my >= cy - halfH' && my <= cy + halfH'

-- Input --------------------------------------------------------

inputKey :: Event -> GameState -> GameState
-- vertical movement
inputKey (EventKey (SpecialKey KeyUp)    Down _ _) gs = gs { inputY =  1 }
inputKey (EventKey (SpecialKey KeyDown)  Down _ _) gs = gs { inputY = -1 }
inputKey (EventKey (SpecialKey KeyUp)      Up _ _) gs = gs { inputY =  0 }
inputKey (EventKey (SpecialKey KeyDown)    Up _ _) gs = gs { inputY =  0 }

-- prevent toggling pause after death
inputKey (EventKey (SpecialKey KeyEnter) Down _ _) gs
  | health gs <= 0 = gs
  | otherwise      = gs { paused = not (paused gs) }

-- hold-to-fire behavior
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gs =
  let (mb, gs') = tryShoot gs
      gs'' = gs' { inputFire = True }
  in maybe gs'' (\b -> gs'' { bullets = b : bullets gs'' }) mb
inputKey (EventKey (SpecialKey KeySpace)   Up _ _) gs = gs { inputFire = False }

-- restart with 'r' when game over
inputKey (EventKey (Char 'r') Down _ _) gs
  | health gs <= 0 = resetGame gs
  | otherwise      = gs

-- click restart button on game over
inputKey (EventKey (MouseButton LeftButton) Down _ (mx,my)) gs
  | health gs <= 0
  , pointInRect (mx,my) (0, restartBtnY, restartBtnW, restartBtnH) = resetGame gs
  | otherwise = gs

-- catch-all
inputKey _ gs = gs

-- Pure world step ---------------------------------------------

stepWorld :: Float -> GameState -> GameState
stepWorld dt gs0 =
  let gs1 = gs0 { timeAccum = timeAccum gs0 + dt }
      gs2 = gs1 { player = stepPlayer dt (inputY gs1) (player gs1) }

      (mbShot, gs2b) = if inputFire gs2 then tryShoot gs2 else (Nothing, gs2)
      bulletsStart   = maybe id (:) mbShot (bullets gs2b)

      bs1 = map (stepBullet dt) bulletsStart
      trailAdd = map (\b -> BulletTrail (bPos b) 0.22) bs1

      bs2 = filter (inBounds . bPos) bs1
      es1 = map (stepEnemy dt (player gs2b)) (enemies gs2b)
      es2 = filter (inBounds . ePos) es1
      (es3, bs3, scDelta, animAdd) = resolveBulletHits es2 bs2
      (dmg, es4, animAdd2) = resolvePlayerHits (player gs2b) es3
      gs3 = gs2b { enemies = es4
                 , bullets = bs3
                 , anims   = tickAnims dt (anims gs2b ++ trailAdd ++ animAdd ++ animAdd2)
                 , score   = score gs2b + scDelta
                 , health  = max 0 (health gs2b - dmg)
                 }
  in spawnEnemies dt gs3

spawnEnemies :: Float -> GameState -> GameState
spawnEnemies dt gs =
  let diff = spawnSchedule (timeAccum gs)
      p = spawnRate diff * dt
      (r, g1)  = randomR (0.0 :: Float, 1.0) (rng gs)
      (e, g2)  = generateEnemy g1 diff
  in if r < p then gs { enemies = e : enemies gs, rng = g2 }
              else gs { rng = g2 }

-- Movement -----------------------------------------------------

stepPlayer :: Float -> Float -> Player -> Player
stepPlayer dt dir pl =
  let Position x y = pPos pl
      vy = dir * pSpeed pl
      ny = clamp (-halfH+40) (halfH-40) (y + vy*dt)
      cd = max 0 (pCooldown pl - dt)
  in pl { pPos = Position x ny, pCooldown = cd }

stepBullet :: Float -> Bullet -> Bullet
stepBullet dt b = b { bPos = addP (bPos b) (Controller.scale dt (bVel b)) }

stepEnemy :: Float -> Player -> Enemy -> Enemy
stepEnemy dt pl e = let e' = aiStep pl e
                    in e' { ePos = addP (ePos e') (Controller.scale dt (eVel e')) }

scale :: Float -> Vec2 -> Vec2
scale s (vx,vy) = (s*vx, s*vy)

inBounds :: Position -> Bool
inBounds (Position x y) = x > -halfW - 60 && x < halfW + 60 && y > -halfH - 60 && y < halfH + 60

-- Animations ---------------------------------------------------

tickAnims :: Float -> [Anim] -> [Anim]
tickAnims dt = foldr step [] where
  step a acc = case a of
    Explosion p ttl   -> let t = ttl - dt in if t > 0 then Explosion p t   : acc else acc
    FlashHUD ttl      -> let t = ttl - dt in if t > 0 then FlashHUD t      : acc else acc
    MuzzleFlash p ttl -> let t = ttl - dt in if t > 0 then MuzzleFlash p t : acc else acc
    BulletTrail p ttl -> let t = ttl - dt in if t > 0 then BulletTrail p t : acc else acc

-- Shooting -----------------------------------------------------

tryShoot :: GameState -> (Maybe Bullet, GameState)
tryShoot gs
  | pCooldown (player gs) > 0 || health gs <= 0 = (Nothing, gs)
  | otherwise =
      let Position x y = pPos (player gs)
          b    = Bullet (Position (x+20) y) (bulletSpeed, 0)
          pl'  = (player gs) { pCooldown = cooldownSecs }
          flash = MuzzleFlash (Position (x+24) y) 0.12
      in (Just b, gs { player = pl', anims = flash : anims gs })

-- AI & spawning ------------------------------------------------

data Difficulty = Difficulty
  { spawnRate  :: Float
  , enemyHP    :: Int
  , enemySpeed :: Float
  }

spawnSchedule :: Float -> Difficulty
spawnSchedule t =
  let rate = 0.7 + min 2.0 (t/30)
      hp   = 1 + floor (t/25)
      spd  = enemyBaseSpeed + min 220 (t*4)
  in Difficulty rate hp spd

aiStep :: Player -> Enemy -> Enemy
aiStep pl e = case eType e of
  Basic   -> e
  Tracker -> let py' = py (pPos pl)
                 ey  = py (ePos e)
                 vy  = (py' - ey) * 0.6
             in e { eVel = (fst (eVel e), vy) }
  Shooter -> e

-- RNG-driven enemy creation
generateEnemy :: StdGen -> Difficulty -> (Enemy, StdGen)
generateEnemy g diff =
  let (y0, g1)    = randomR (-halfH+40, halfH-40) g
      (ix, g2)    = randomR (0, 9 :: Int) g1
      eType'      = if ix < 6 then Basic else if ix < 9 then Tracker else Shooter
      baseVel     = (- enemySpeed diff, 0)
      e           = Enemy { ePos = Position (halfW - 20) y0
                          , eVel = baseVel
                          , eHP  = enemyHP diff
                          , eType = eType'
                          }
  in (e, g2)

-- Collisions ---------------------------------------------------

resolveBulletHits :: [Enemy] -> [Bullet] -> ([Enemy], [Bullet], Int, [Anim])
resolveBulletHits es bs = go es bs 0 [] where
  go [] bs' sc anims' = ([], bs', sc, anims')
  go (e:rest) bs' sc anims' =
    let (hit, bsAfter) = consumeOneHit e bs'
    in if hit
         then
           let e' = e { eHP = eHP e - 1 }
               (esNext, bsNext, sc', anims'') = go rest bsAfter sc anims'
               died      = eHP e' <= 0
               animsAdd  = if died then [Explosion (ePos e) 0.35] else []
               scAdd     = if died then 10 else 0
               esFinal   = if died then esNext else e' : esNext
           in (esFinal, bsNext, sc' + scAdd, anims'' ++ animsAdd)
         else
           let (esNext, bsNext, sc', anims'') = go rest bs' sc anims'
           in (e:esNext, bsNext, sc', anims'')

  consumeOneHit :: Enemy -> [Bullet] -> (Bool, [Bullet])
  consumeOneHit enemy bsList =
    case break (\b -> hitCircle (ePos enemy) enemyRadius (bPos b) bulletRadius) bsList of
      (left, _hit:right) -> (True, left ++ right)
      _                  -> (False, bsList)

resolvePlayerHits :: Player -> [Enemy] -> (Int, [Enemy], [Anim])
resolvePlayerHits pl = foldr step (0, [], []) where
  step e (dmg, accE, accA)
    | hitCircle (pPos pl) playerRadius (ePos e) enemyRadius
    = (dmg+1, accE, Explosion (ePos e) 0.35 : accA)
    | otherwise = (dmg, e:accE, accA)
