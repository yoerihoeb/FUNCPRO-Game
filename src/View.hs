-- | Rendering
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view gs = pure $ Pictures
  [ translate 0 0 (color (greyN 0.1) (rectangleSolid screenW screenH))
  , drawStars (round (timeAccum gs))
  , drawPlayer (player gs)
  , Pictures (map drawEnemy (enemies gs))
  , Pictures (map drawBullet (bullets gs))
  , Pictures (map drawAnim (anims gs))
  , drawHUD gs
  , if paused gs then pausedOverlay else Blank
  , if health gs <= 0 then gameOverOverlay (score gs) else Blank
  ]

-- Simple parallax stars based on time seed
drawStars :: Int -> Picture
drawStars seed = Pictures [ star (-400 + fromIntegral ((37*seed) `mod` 800)) 300
                          , star 200 (-150)
                          , star (-100) (-220)
                          , star 350 50
                          , star 0 120
                          ]
  where star sx sy = translate sx sy (color (greyN 0.6) (thickCircle 1 1))

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
drawAnim (FlashHUD ttl) = Color (withAlpha (ttl/0.5) yellow) (rectangleSolid 20 20)

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
  ]
