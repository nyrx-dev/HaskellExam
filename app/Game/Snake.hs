module Game.Snake where

import Graphics.Gloss.Interface.Pure.Game
import Scratchy.Syntax

-- Start eksæmpel:
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

-- Spiller keybindings
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

-- Checker om det er modsatte retninger
isOpposite :: Dir -> Dir -> Bool
isOpposite U D = True
isOpposite D U = True
isOpposite L R = True
isOpposite R L = True
isOpposite _ _ = False

-- Laver slange kroppen
makeBody :: Color -> Cell -> Int -> SpritePtr -> [Cell] -> SProg ()
makeBody _ _ 0 _ _ = Pure () -- No more segments
makeBody col startPos n prevPtr prevCells =
    -- Start each segment below the previous
    let startCell' = case prevCells of
            (c : _) -> nextCell D c
            [] -> iterate (nextCell D) startPos !! n
     in NewSprite
            startCell'
            (Color col $ rectangleSolid (cellSize * 0.95) (cellSize * 0.95))
            ( \bodyPtr ->
                -- Each segment follows the previous segment's last cell
                OnTargetUpdated prevPtr (\oldCell _newCell -> SetTarget bodyPtr oldCell (Pure ())) $
                    makeBody col startPos (n - 1) bodyPtr (startCell' : prevCells)
            )


-- Hoved controller
headController :: SpritePtr -> [(Key, Dir)] -> SProg ()
headController headPtr keys = controlHead Nothing
  where
    controlHead :: Maybe Dir -> SProg ()
    controlHead currentDir =
        let
            updateDir :: Dir -> SProg ()
            updateDir newDir =
                OnTargetReached
                    headPtr
                    ( \cl ->
                        -- COLLISION CHECK
                        let next = nextCell newDir cl
                         in InspectCell
                                next
                                ( \result ->
                                    case result of
                                        IsFree -> SetTarget headPtr next (SetBackgroundColor white (Pure ()))
                                        HasBarrier -> SetBackgroundColor red $ Pure ()
                                        HasSprite -> SetBackgroundColor black $ Pure ()
                                )
                    )
                    $ controlHead (Just newDir)

            handleDir :: Dir -> SProg ()
            handleDir dir =
                case currentDir of
                    Nothing -> updateDir dir
                    Just d -> if isOpposite d dir then Pure () else updateDir dir
         in
            foldr (\(k, d) acc -> OnKeyEvent k (handleDir d) acc) (Pure ()) keys

-- Laver en slange sprite
snakeSprite :: [(Key, Dir)] -> Color -> Cell -> SProg ()
snakeSprite keys col startPos =
    NewSprite
        startPos
        (Color col $ circleSolid (cellSize * 0.5))
        ( \headPtr ->
            headController headPtr keys
                `combine` makeBody col startPos 7 headPtr [startPos]
        )

-- Kombinerer 2 slanger, med fixed positioner og farver
snake :: SProg ()
snake =
    snakeSprite player1Keys green (10, 15)
        `combine` snakeSprite player2Keys blue (20, 15)
