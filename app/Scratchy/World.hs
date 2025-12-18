module Scratchy.World where

import Control.Monad
import Data.List (partition)
import Graphics.Gloss.Interface.Pure.Game

import Scratchy.Syntax

-- Some useful helpers
------------------------------------------------------------

imapM :: (Applicative m) => (Int -> a -> m b) -> [a] -> m [b]
imapM f = zipWithM f [0 ..]

imapM_ :: (Applicative m) => (Int -> a -> m b) -> [a] -> m ()
imapM_ f xs = () <$ zipWithM f [0 ..] xs

setAt :: Int -> a -> [a] -> [a]
setAt i x xs = zipWith (\j y -> if i == j then x else y) [0 ..] xs

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- Sprites
------------------------------------------------------------

data Sprite = Sprite
    { pic :: Picture
    , pos :: Pos
    , tgt :: Cell
    }
    deriving (Show, Eq)

-- World
------------------------------------------------------------

data World = World
    { bg :: Color
    , sprites :: ![Sprite]
    , keysHeld :: ![Key]
    , prog :: !(SProg ()) -- Current program
    , keyHdlrs :: ![(Key, SProg ())] -- Key pressed
    , trHdlrs ::
        ![ ( SpritePtr
           , Cell ->
             SProg () -- Target reached
           )
         ]
    , tuHdlrs ::
        ![ ( SpritePtr
           , Cell ->
             Cell ->
             SProg () -- Target updated
           )
         ]
    , bhHdlrs ::
        ![ ( SpritePtr
           , Cell ->
             Cell ->
             SProg () -- Barrier hit
           )
         ]
    , timers :: ![(Duration, SProg ())] -- Timers
    }

-- Game
------------------------------------------------------------

newtype Game a = Game {runGame :: World -> (World, a)}
    deriving (Functor)

instance Applicative Game where
    pure x = Game $ \w -> (w, x)
    (<*>) = ap

instance Monad Game where
    Game f >>= k = Game $ \w ->
        let (w', x) = f w in runGame (k x) w'

-- Sprite movement ops
------------------------------------------------------------

moveSprites :: Float -> Game ()
moveSprites dt = Game $ \w@World{..} ->
    let (w', sprites') = flip runGame w $ imapM (moveSprite dt) sprites
     in ( w'{sprites = sprites'}
        , ()
        )

moveSprite :: Float -> SpritePtr -> Sprite -> Game Sprite
moveSprite dt ptr m@Sprite{..} = Game $ \w@World{..} ->
    let (tx, ty) = tgt
        (px, py) = pos
        dx = fromIntegral tx - px
        dy = fromIntegral ty - py
        dist = sqrt (dx * dx + dy * dy)
        stepSize = cellsPerSec * dt
     in if dist <= stepSize * dt
            then
                ( w
                    { prog =
                        prog
                            `combine` ( case lookup ptr trHdlrs of
                                            Just f -> f tgt
                                            Nothing -> Pure ()
                                      )
                    }
                , m{pos = (fromIntegral tx, fromIntegral ty)}
                )
            else
                ( w
                , m
                    { pos =
                        ( px + (stepSize / dist) * dx
                        , py + (stepSize / dist) * dy
                        )
                    }
                )

setSpriteTarget :: SpritePtr -> Cell -> Game ()
setSpriteTarget ptr c = Game $ \w@World{..} ->
    let spr = sprites !! ptr
     in if inBounds c
            then
                ( w
                    { sprites =
                        setAt
                            ptr
                            (spr{tgt = c})
                            sprites
                    , prog =
                        prog
                            `combine` ( case lookup ptr tuHdlrs of
                                            Just f -> f (tgt spr) c
                                            Nothing -> Pure ()
                                      )
                    }
                , ()
                )
            else
                ( w
                    { prog =
                        prog
                            `combine` ( case lookup ptr bhHdlrs of
                                            Just f -> f (tgt spr) c
                                            Nothing -> Pure ()
                                      )
                    }
                , ()
                )

spriteExistsAt :: Cell -> [Sprite] -> Maybe SpritePtr
spriteExistsAt c = go . zip [0 ..]
  where
    go [] = Nothing
    go ((p, s) : sprs) =
        let (p1, p2) = pos s
            c' = (round p1, round p2)
         in if c == c' || c == tgt s then Just p else go sprs

-- Input handling
------------------------------------------------------------

handleEvent :: Event -> Game ()
handleEvent (EventKey c d _ _) = case d of
    Down -> press c
    Up -> release c
handleEvent _ = pure ()

press :: Key -> Game ()
press d = Game $ \w@World{..} ->
    ( w{keysHeld = d : filter (/= d) keysHeld}
    , ()
    )

release :: Key -> Game ()
release d = Game $ \w@World{..} ->
    ( w{keysHeld = filter (/= d) keysHeld}
    , ()
    )

handleKeyEvents :: Game ()
handleKeyEvents = Game $ \w@World{..} ->
    ( fst $
        flip runGame w $
            mapM_ handleKey keysHeld
    , ()
    )

handleKey :: Key -> Game ()
handleKey k = Game $ \w@World{..} ->
    ( w
        { prog =
            prog
                `combine` ( case lookup k keyHdlrs of
                                Just m -> m
                                Nothing -> Pure ()
                          )
        }
    , ()
    )

-- Timers
------------------------------------------------------------

tickTimers :: Game ()
tickTimers = Game $ \w@World{..} ->
    let ts = map (\(d, m) -> (d - 1, m)) timers
        (done, remaining) = partition (\(d, _) -> d <= 0) ts
        p' = foldr (\(_, m) -> combine m) (Pure ()) done
     in ( w
            { timers = remaining
            , prog = prog `combine` p'
            }
        , ()
        )

-- Running programs
------------------------------------------------------------

runProg :: Game ()
runProg = Game $ \w -> (go (prog w) w{prog = Pure ()}, ())
  where
    go :: SProg () -> World -> World
    go program world = case program of
        Pure () -> world
        Combine first second ->
            let world' = go first world
             in go second world'
        OnKeyEvent key handler rest ->
            let world' = world{keyHdlrs = (key, handler) : keyHdlrs world}
             in go rest world'
        OnTargetReached sprite handler rest ->
            let world' = world{trHdlrs = (sprite, handler) : trHdlrs world}
             in go rest world'
        OnTargetUpdated sprite handler rest ->
            let world' = world{tuHdlrs = (sprite, handler) : tuHdlrs world}
             in go rest world'
        OnBarrierHit sprite handler rest ->
            let world' = world{bhHdlrs = (sprite, handler) : bhHdlrs world}
             in go rest world'
        NewSprite cell picture continuation ->
            let newSprite = Sprite{pic = picture, pos = cellToPos cell, tgt = cell}
                spriteIndex = length (sprites world)
                world' = world{sprites = sprites world ++ [newSprite]}
                nextProgram = continuation spriteIndex
             in go nextProgram world'
        SetColor sprite col rest ->
            let currentSprite = sprites world !! sprite
                updatedSprite = currentSprite{pic = Color col (pic currentSprite)}
                world' = world{sprites = setAt sprite updatedSprite (sprites world)}
             in go rest world'
        SetTarget sprite cell rest ->
            let (world', ()) = runGame (setSpriteTarget sprite cell) world
             in go rest world'
        GetTarget sprite continuation ->
            let targetCell = tgt (sprites world !! sprite)
                nextProgram = continuation targetCell
             in go nextProgram world
        SetBackgroundColor col rest ->
            let world' = world{bg = col}
             in go rest world'
        InspectCell cell continuation ->
            let result = inspectCellAt cell world
                nextProgram = continuation result
             in go nextProgram world
        After duration handler rest ->
            let world' = world{timers = (duration, handler) : timers world}
             in go rest world'

    cellToPos :: Cell -> Pos
    cellToPos (cx, cy) = (fromIntegral cx, fromIntegral cy)

    inspectCellAt :: Cell -> World -> InspectionResult
    inspectCellAt cell world
        | not (inBounds cell) = HasBarrier
        | spriteAt cell = HasSprite
        | otherwise = IsFree
      where
        spriteAt c = case spriteExistsAt c (sprites world) of
            Just _ -> True
            Nothing -> False
