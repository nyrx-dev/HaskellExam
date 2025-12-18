module Example.CircleThatMoves where

import Graphics.Gloss.Interface.Pure.Game
import Scratchy.Syntax

circSprite :: SProg ()
circSprite =
    NewSprite
        (15, 15)
        (Color green $ circleSolid (cellSize * 0.6))
        ( \s ->
            OnKeyEvent
                (Char 'w')
                ( OnTargetReached
                    s
                    (\cl -> SetTarget s (nextCell U cl) (Pure ()))
                    $ Pure ()
                )
                $ OnKeyEvent
                    (Char 's')
                    ( OnTargetReached
                        s
                        (\cl -> SetTarget s (nextCell D cl) (Pure ()))
                        $ Pure ()
                    )
                $ Pure ()
        )
        
player1Keys :: [(Key, Dir)]
player1Keys =
    [ (Char 'w', U)
    , (Char 's', D)
    , (Char 'a', L)
    , (Char 'd', R)
    ]

player2Keys :: [(Key, Dir)]
player2Keys =
    [ (Char 'i', U)
    , (Char 'k', D)
    , (Char 'j', L)
    , (Char 'l', R)
    ]

-- Sprite with a circle head and 7 squares as its body, arranged vertically
spriteWithCircleHeadAndSquares :: [(Key, Dir)] -> Color -> Cell -> SProg ()
spriteWithCircleHeadAndSquares keys col startPos =
    NewSprite startPos
        (Color col $ circleSolid (cellSize * 0.6))
        (\headPtr ->
            let
                makeBody :: Int -> SpritePtr -> [Cell] -> SProg ()
                makeBody 0 _ _ = Pure () -- No more segments
                makeBody n prevPtr prevCells =
                    -- Start each segment below the previous
                    let startCell' = case prevCells of
                                        (c:_) -> (fst c, snd c - 1)
                                        []    -> let (sx, sy) = startPos in (sx, sy - n)
                    in NewSprite startCell'
                            (Color col $ rectangleSolid (cellSize * 0.8) (cellSize * 0.8))
                            (\bodyPtr ->
                                -- Each segment follows the previous segment's last cell
                                OnTargetUpdated prevPtr (\oldCell _newCell -> SetTarget bodyPtr oldCell (Pure ())) $
                                makeBody (n-1) bodyPtr (startCell':prevCells)
                            )
                -- Head movement
                moveHead =
                    let
                        isOpposite :: Dir -> Dir -> Bool
                        isOpposite U D = True
                        isOpposite D U = True
                        isOpposite L R = True
                        isOpposite R L = True
                        isOpposite _ _ = False

                        controlHead :: Maybe Dir -> SProg ()
                        controlHead currentDir =
                            let
                                updateDir :: Dir -> SProg ()
                                updateDir newDir =
                                    OnTargetReached headPtr (\cl -> 
                                        -- COLLISION CHECK GOES HERE:
                                        -- 1. Calculate the next cell: `let next = nextCell newDir cl`
                                        -- 2. Check that cell: `InspectCell next (\result -> ...)`
                                        -- 3. Based on `result` (IsFree, HasBarrier, HasSprite), decide whether to move or end game.
                                        let next = nextCell newDir cl in
                                        InspectCell next (\result ->
                                            case result of
                                                IsFree -> SetTarget headPtr next (SetBackgroundColor white (Pure ()))
                                                HasBarrier -> SetBackgroundColor red $ Pure ()
                                                HasSprite -> SetBackgroundColor black $ Pure ()
                                        )
                                    ) $
                                    controlHead (Just newDir)
                                
                                handleDir :: Dir -> SProg ()
                                handleDir dir =
                                    case currentDir of
                                        Nothing -> updateDir dir
                                        Just d -> if isOpposite d dir then Pure () else updateDir dir
                            in
                                foldr (\(k, d) acc -> OnKeyEvent k (handleDir d) acc) (Pure ()) keys
                    in
                        controlHead Nothing
            in
                moveHead `combine` (makeBody 7 headPtr [startPos])
        )

snake :: SProg ()
snake =
    spriteWithCircleHeadAndSquares player1Keys blue (10, 15)
    `combine`
    spriteWithCircleHeadAndSquares player2Keys orange (20, 15)

