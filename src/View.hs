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
  , Pictures (map drawEnemyBullet (enemyBullets gs))
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
drawPlayer pl =
  translate x y $ Pictures
    [ -- soft cyan aura
      Color (withAlpha 0.20 (makeColor 0.3 0.9 1.0 1.0)) (thickCircle 14 18)

      -- body (two-tone to fake depth)
    , Color (makeColor 0.10 0.65 0.90 1.0) $
        Polygon [(-16,-12),(16,0),(-16,12)]
    , Color (makeColor 0.05 0.45 0.70 1.0) $
        Polygon [(-12,-8),(10,0),(-12,8)]

      -- cockpit bubble
    , translate (-4) 0 $
        Color (makeColor 0.85 0.95 1.0 0.95) $
          scale 0.7 0.7 (circleSolid 6)

      -- wing tips
    , Color (makeColor 0.15 0.85 1.0 1.0) $
        Pictures [ translate (-10)  8 (rectangleSolid 8 2)
                 , translate (-10) (-8) (rectangleSolid 8 2)
                 ]

      -- engine flame
    , let pulse = clp (1 - pCooldown pl / cooldownSecs)
          len   = 10 + 18 * pulse
          wid   = 6  +  6 * pulse
      in translate (-18) 0 $ Pictures
           [ Color (withAlpha (0.75* (0.6 + 0.4*pulse)) (makeColor 1.0 0.8 0.2 1.0))
               (Polygon [(0,-wid/2),(-len,0),(0,wid/2)])
           , Color (withAlpha (0.45* (0.6 + 0.4*pulse)) (makeColor 1.0 0.4 0.1 1.0))
               (Polygon [(0,-wid*0.6/2),(-len*0.7,0),(0,wid*0.6/2)])
           ]
    ]
  where
    Position x y = pPos pl

drawEnemy :: Enemy -> Picture
drawEnemy e = translate x y $ case eType e of
  Basic   -> basicBot
  Tracker -> trackerBot
  Shooter -> shooterBot
  where
    Position x y = ePos e

    -- BASIC: glossy red drone with a center “eye”
    basicBot =
      Pictures
        [ Color (makeColor 0.65 0.05 0.05 1.0) (roundedRect 32 26 5)
        , Color (withAlpha 0.3 white) (roundedRect 32 26 5)        
        , highlightStrip 32 26
        , Color (makeColor 0.95 0.2 0.2 1.0) (roundedRect 26 20 4)
        , translate 0 0 $ Color (makeColor 0.98 0.95 0.95 1.0) (circleSolid 4)
        , Color (makeColor 0.15 0.02 0.02 1.0) (rectangleWire 32 26)
        ]

    -- TRACKER: orange fins + side vents (more agile vibe)
    trackerBot =
      Pictures
        [ Color (makeColor 0.90 0.45 0.10 1.0) (roundedRect 30 22 5)
        , Color (makeColor 1.0 0.65 0.25 1.0) (roundedRect 24 16 4)
        , Color (withAlpha 0.25 white) (roundedRect 30 22 5)
        , highlightStrip 30 22
        , Color (makeColor 0.65 0.20 0.05 1.0) $
            Pictures [ translate (-18)   0 (Polygon [ (0,-6),(-8,0),(0,6) ])
                     , translate ( 18)   0 (Polygon [ (0,-6),( 8,0),(0,6) ])
                     ]
        , Color (makeColor 0.15 0.05 0.02 1.0) (rectangleWire 30 22)
        ]

    -- SHOOTER: purple gunship with a front turret slit
    shooterBot =
      Pictures
        [ Color (makeColor 0.45 0.15 0.70 1.0) (roundedRect 36 24 6)
        , Color (makeColor 0.60 0.25 0.90 1.0) (roundedRect 28 18 5)
        , Color (withAlpha 0.25 white) (roundedRect 36 24 6)
        , highlightStrip 36 24
        , translate 6 0 $ Color (makeColor 0.1 0.1 0.2 1.0) (rectangleSolid 10 4)
        , Color (makeColor 0.12 0.04 0.18 1.0) (rectangleWire 36 24)
        ]

drawBullet :: Bullet -> Picture
drawBullet b = translate x y $ Color white $ circleSolid 4
  where Position x y = bPos b

drawEnemyBullet :: Bullet -> Picture
drawEnemyBullet b =
  let Position x y = bPos b
  in translate x y $ Pictures
       [ Color (withAlpha 0.8 (makeColor 1.0 0.35 0.25 1.0)) (circleSolid 4)
       , Color (withAlpha 0.4 (makeColor 1.0 0.2  0.1  1.0)) (thickCircle 2 6)
       ]

drawAnim :: Anim -> Picture
drawAnim (Explosion (Position ax ay) ttl) =
  let k = ttl / 0.35
  in translate ax ay $ Color (withAlpha (k) orange) $ thickCircle (8*(1-k)) 12

drawAnim (FlashHUD (Position ax ay) ttl) =
  let k = ttl / 0.25
      r = 28 * (1 - k * 0.6)
  in translate ax ay $
       Color (withAlpha (0.5 * k) (makeColor 1.0 0.8 0.2 1.0)) (circleSolid r)

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

 -- Small drawing helpers -------------------------------------------------------

roundedRect :: Float -> Float -> Float -> Picture
roundedRect w h r =
  let core = rectangleSolid (w - 2*r) (h - 2*r)
      corner dx dy = translate dx dy (circleSolid r)
  in Pictures
      [ core
      , corner ( w/2 - r) ( h/2 - r)
      , corner (-w/2 + r) ( h/2 - r)
      , corner ( w/2 - r) (-h/2 + r)
      , corner (-w/2 + r) (-h/2 + r)
      ]

-- Simple “specular” highlight strip (used on enemies)
highlightStrip :: Float -> Float -> Picture
highlightStrip w h =
  translate (-w*0.15) (h*0.12) $
    Color (withAlpha 0.3 white) $ rectangleSolid (w*0.2) (h*0.7)

-- Clamp helper
clp :: Float -> Float
clp x = max 0 (min 1 x)
 
