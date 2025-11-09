# 🚀 Shoot ’Em Up — Haskell Gloss Game

A simple 2D side-scrolling shoot ’em up built with **Haskell** and **Gloss**.  
You pilot a ship through waves of enemies, dodging bullets, scoring points, and racking up high scores saved to a file.

---

## 🧱 Build & Run Instructions

### Requirements
- GHC ≥ 9.0  
- Cabal or Stack  
- Libraries:  
  - `gloss`  
  - `gloss-juicy` 
  - `random`

### Run (Cabal)
```bash
cabal build
cabal run
```

The game window opens automatically (`1024×720`).

---

## 🎮 Controls

| Key / Action | Description |
|---------------|-------------|
| **Up / Down** | Move the player ship vertically |
| **Space**     | Shoot — hold to autofire |
| **Enter**     | Pause / resume |
| **R**         | Restart after Game Over |
| **Mouse click** | Click “Restart” button on Game Over screen |
| **Esc / close window** | Exit the game |

---

## ✨ Gameplay Features

- **Player ship** with smooth movement, muzzle flash, and engine trail  
- **Three enemy types:**
  - **Basic:** slow, straight-line drones  
  - **Tracker:** moves vertically toward the player  
  - **Shooter:** fires straight ahead periodically  
- **Dynamic difficulty** – spawn rate, speed, and HP increase with time  
- **Scrolling starfield** background for depth  
- **Explosions, bullet trails, impact flashes** for hits  
- **Pause menu** (hidden during Game Over)  
- **Restart** via key or clickable button  

---

## 🧠 Code Structure

| File | Purpose |
|------|----------|
| `Main.hs` | Launches the Gloss game (`playIO`) |
| `Model.hs` | Data definitions and constants |
| `Controller.hs` | Input handling, game logic, collisions, file I/O |
| `View.hs` | Rendering of player, enemies, bullets, animations, overlays |
| `highscores.txt` | Persistent data file |
---

## 🧩 Architecture

- **Functional core, imperative shell**:  
  - Pure logic in `Controller` and `Model`  
  - `IO` only in `Main` and the high-score section
- **State update loop:** `step :: Float -> GameState -> IO GameState`
- **Rendering loop:** `view :: GameState -> IO Picture`

---

## 📦 Notes

- High scores are stored as plain text for easy inspection.  
- The RNG state is preserved between frames for deterministic spawning.  
- The game auto-saves high scores; no manual saving required.  

---

## 🧑‍💻 Author
**Simon de Jong & Yoeri Hoebe**  
Haskell Programming — Functional Programming Project  
