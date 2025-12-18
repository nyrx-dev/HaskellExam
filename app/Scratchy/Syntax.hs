module Scratchy.Syntax where

import Graphics.Gloss.Interface.Pure.Game

-- Types
------------------------------------------------------------

type Duration = Int
type Pos = (Float, Float)
type Cell = (Int, Int)
type SpritePtr = Int

-- Constants
------------------------------------------------------------

cellsPerSec :: Float
cellsPerSec = 5

gridW, gridH :: Int
gridW = 30
gridH = 30 -- FIXME: move.

inBounds :: (Int, Int) -> Bool
inBounds (x, y) = x >= 0 && x < gridW && y >= 0 && y < gridH

cellSize :: Float
cellSize = 30

winW, winH :: Int
winW = round (fromIntegral gridW * cellSize)
winH = round (fromIntegral gridH * cellSize)

-- Direction
------------------------------------------------------------

data Dir = U | D | L | R deriving (Eq, Ord, Show)

dirVec :: Dir -> (Int, Int)
dirVec U = (0, 1)
dirVec D = (0, -1)
dirVec L = (-1, 0)
dirVec R = (1, 0)

nextCell :: Dir -> (Int, Int) -> (Int, Int)
nextCell d (cx, cy) =
    let (dx, dy) = dirVec d
     in (cx + dx, cy + dy)

-- Syntax
------------------------------------------------------------

data InspectionResult
    = HasBarrier
    | HasSprite
    | IsFree

data SProg a
    = Pure a
    | Combine (SProg ()) (SProg a)
    | -- Event listening
      OnKeyEvent Key (SProg ()) (SProg a) -- Listener for a specific key
    | OnTargetReached SpritePtr (Cell -> SProg ()) (SProg a) -- Listener for a specific sprite
    | OnTargetUpdated SpritePtr (Cell -> Cell -> SProg ()) (SProg a) -- Listener for a specific sprite
    | OnBarrierHit SpritePtr (Cell -> Cell -> SProg ()) (SProg a)
    | -- Sprite and grid actions
      NewSprite Cell Picture (SpritePtr -> SProg a)
    | SetColor SpritePtr Color (SProg a)
    | SetTarget SpritePtr Cell (SProg a)
    | GetTarget SpritePtr (Cell -> SProg a)
    | SetBackgroundColor Color (SProg a)
    | InspectCell Cell (InspectionResult -> SProg a)
    | -- Timer
      After Duration (SProg ()) (SProg a)
    deriving (Functor)

-- Combining programs
------------------------------------------------------------

combine :: SProg () -> SProg () -> SProg ()
combine = Combine
