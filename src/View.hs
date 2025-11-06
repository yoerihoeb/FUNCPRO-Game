-- | Rendering
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view gs = pure $ Pictures
  [ translate 0 0 (color (greyN 0.1) (rectangleSolid screenW screenH))
  , drawStars (timeAccum gs)
  , drawPlayer (player gs)
  , Pictures (map drawEnemy (enemies gs))
  , Pictures (map drawBullet (bullets gs))
  , Pictures (map drawAnim (anims gs))
  , drawHUD gs
  , if paused gs && health gs > 0 then pausedOverlay else Blank
  , if health gs <= 0 then gameOverOverlay (score gs) else Blank
  ]

-- Simple parallax stars based on time seed
drawStars :: Float -> Picture
drawStars t =
  Pictures [ layer 0.3 80, layer 0.6 60, layer 1.0 40 ]
  where
    layer speed n = Pictures [ star i speed | i <- [1..n] ]
    star i speed =
      let x = (fromIntegral ((i * 97) `mod` 2000) - 1000) + speed * t * (-50)
          y = fromIntegral ((i * 173) `mod` 1440) - 720
          x' = if x < -512 then x + 1024 else x
      in translate x' y $ color (greyN (0.5 + 0.5 * sin (t/3 + fromIntegral i))) (circleSolid 1.5)

-- Entities -----------------------------------------------------

drawPlayer :: Player -> Picture
drawPlayer pl = translate x y $ Color azure $ Pictures
  [ Polygon [(-15,-15),(20,0),(-15,15)]
  , Translate (-12) 0 (rectangleSolid 4 4)
  ]
  where Position x y = pPos pl

drawEnemy :: Enemy -> Picture
drawEnemy e = translate x y $ Color red $ rectangleSolid 28 22
  where Position x y = ePos e

drawBullet :: Bullet -> Picture
drawBullet b = translate x y $ Color white $ circleSolid 4
  where Position x y = bPos b

drawAnim :: Anim -> Picture
drawAnim (Explosion (Position ax ay) ttl) =
  let k = ttl / 0.35
  in translate ax ay $ Color (withAlpha (k) orange) $ thickCircle (8*(1-k)) 12

drawAnim (FlashHUD ttl) =
  Color (withAlpha (ttl/0.5) yellow) (rectangleSolid 20 20)

drawAnim (MuzzleFlash (Position ax ay) ttl) =
  let k = ttl / 0.12              -- 1 → 0 over lifetime
      w = 12 * k                  -- width shrinks
      l = 24 * k                  -- length shrinks
      poly = Polygon [(0,-w/2),(l,0),(0,w/2)]
  in translate ax ay $ Pictures
       [ Color (withAlpha (0.9*k) white) poly
       , Color (withAlpha (0.7*k) yellow) (scale 0.7 0.7 poly)
       ]

drawAnim (BulletTrail (Position ax ay) ttl) =
  let k = ttl / 0.22
      r = 3 + 2*(1-k)
  in translate ax ay $ Color (withAlpha (0.5*k) (greyN 0.9)) (circleSolid r)

-- HUD & overlays ----------------------------------------------
drawHUD :: GameState -> Picture
drawHUD gs = Pictures
  [ translate (-screenW/2 + 20) (screenH/2 - 40) $ scale 0.15 0.15 $ color white $ text ("Score: " ++ show (score gs))
  , translate (screenW/2 - 180) (screenH/2 - 40) $ scale 0.15 0.15 $ color white $ text ("Health: " ++ show (health gs))
  ]

pausedOverlay :: Picture
pausedOverlay = Pictures
  [ Color (withAlpha 0.4 black) (rectangleSolid screenW screenH)
  , translate (-120) 0 $ scale 0.3 0.3 $ color white $ text "Paused"
  ]

gameOverOverlay :: Int -> Picture
gameOverOverlay sc = Pictures
  [ Color (withAlpha 0.6 black) (rectangleSolid screenW screenH)
  , translate (-180) 20  $ scale 0.35 0.35 $ color white $ text "Game Over"
  , translate (-250) (-60) $ scale 0.18 0.18 $ color white $ text ("Final score: " ++ show sc)
  , translate 0 restartBtnY $ Pictures
      [ Color (withAlpha 0.15 white) (rectangleSolid restartBtnW restartBtnH)
      , Color white $ rectangleWire restartBtnW restartBtnH
      , translate (-80) (-14) $ scale 0.18 0.18 $ color white $ text "Restart (R)"
      ]
  ]
