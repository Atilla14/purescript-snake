module Main where

import Data.Array
import Data.Maybe
import FRP.Event.Class
import Halogen.VDom
import Math
import Prelude
import Types
import UI.Elements
import UI.Events
import UI.Properties

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Plus ((<|>))
import Data.Int (fromNumber, toNumber)
import Data.List.NonEmpty (mapWithIndex)
import FRP.Behavior as B
import FRP.Behavior.Keyboard (keys)
import FRP.Behavior.Keyboard (keys, key)
import FRP.Event as E
import FRP.Event.Time (animationFrame, interval)
import Partial.Unsafe (unsafePartial)
import UI.Core (MEvent, AttrValue(..), Attr(..), Prop)
import UI.Util as U

foreign import click :: MEvent
foreign import move :: E.Event { keyCode :: Int }
foreign import randomInt :: Int -> Int -> Int

debugger = true

----------------- Global Constants -------------------

-- Box -- ViewBox Size in actual pixels
boxHeight :: Number
boxHeight = 600.0

boxWidth :: Number
boxWidth = 800.0

-- Grid -- Snake Grid 
gridHeight :: Int
gridHeight = 30

gridWidth :: Int
gridWidth = 40

-- Cell -- Single cell in snake grid
cellHeight :: Number
cellHeight = boxHeight / (toNumber gridHeight)

cellWidth :: Number
cellWidth = boxWidth / (toNumber gridWidth)

--------------------- Types --------------------------

data Direction = Left | Right | Top | Bottom

instance directionShow :: Show Direction where
  show (Left)   = "Left"
  show (Right)  = "Right"
  show (Top)    = "Top"
  show (Bottom) = "Bottom"

type Position = { x :: Int, y :: Int }

type FoodCell = { x :: Int , y :: Int , value :: Int , timeout :: Int }

type SnakeCell = { x :: Int , y :: Int , nextPos :: { x :: Int , y :: Int } }

data Cell = FC FoodCell | SC SnakeCell

type SnakeState = { cells :: Array SnakeCell , size :: Int , alive :: Boolean , direction :: Direction }

type FoodState = { cells :: Array FoodCell , count :: Int }

type GameState = { snake :: SnakeState , food :: FoodState }

initialState :: GameState
initialState = { snake : { cells: replicate 1 { x: 5 , y: 5 , nextPos: { x: 6 , y: 5 } }
                         , size: 1
                         , alive: true
                         , direction: Right
                         }
                , food : { cells: [ { x: 10 , y: 20 , value: 1 , timeout: 10 } ]
                         , count: 1
                         }
                }
              
-- View --

toMargin :: Int -> Int -> String
toMargin x y = (show $ (toNumber x) * cellWidth) <> "," <> (show $ (toNumber y) * cellHeight) <> ",0,0"

widget :: forall i. GameState -> VDom Attr i
widget state = linearLayout
               [ id_ "app"
               , height "match_parent"
               , width "match_parent"
               , background "#16191B"
               , gravity "center"
               ]
               [ relativeLayout
                 [ id_ "game"
                 , height (show boxHeight)
                 , width (show boxWidth)
                 , background "#16191B"
                 , stroke "2, #f0f0f0"
                 ] [ food state.food , snake state.snake ]

               , linearLayout
                 [ id_ "score" 
                 , height "100"
                 , width "200"
                 , gravity "center"
                 ]
                 [ textView
                   [ id_ "scoreTxt"
                   , height "match_parent"
                   , width "match_parent"
                   , text (show state.snake.size)
                   , gravity "center"
                    ] 
                   ]
                 ]

snake :: forall i. SnakeState -> VDom Attr i
snake state = relativeLayout
              [ id_ "snake"
              , height "match_parent"
              , width "match_parent"
              ] (mapWithIndex snakeCell state.cells)

food :: forall i. FoodState -> VDom Attr i
food state  = relativeLayout
              [ id_ "food_layer"
              , height "match_parent"
              , width "match_parent"
              ] (mapWithIndex foodCell state.cells)

snakeCell :: forall i. Int -> SnakeCell -> VDom Attr i
snakeCell index state = linearLayout
                        [ id_ ("snake" <> (show index))
                        , className "snake-cell"
                        , height (show cellHeight)
                        , width (show cellWidth)
                        , background "#e2e6e8"
                        , stroke "2, #16191B"
                        , margin (toMargin state.x state.y)
                        ] []

foodCell :: forall i. Int -> FoodCell -> VDom Attr i
foodCell index state = linearLayout
                       [ id_ ("food" <> (show index))
                       , height (show cellHeight)
                       , width (show cellWidth)
                       , background "#e84118"
                       , stroke "2, #16191B"
                       , margin (toMargin state.x state.y)
                       ] []

-- Update --

getDirection :: Int -> Direction -> Direction
getDirection key default
  | key == 37 || key == 72 || key == 65 = Left
  | key == 38 || key == 75 || key == 87 = Top
  | key == 39 || key == 76 || key == 68 = Right
  | key == 40 || key == 74 || key == 83 = Bottom
  | otherwise = default

normaliseDirection :: Direction -> Direction -> Direction
normaliseDirection Left Right = Right
normaliseDirection Right Left = Left
normaliseDirection Top Bottom = Bottom
normaliseDirection Bottom Top = Top
normaliseDirection dir _ = dir

update :: Int -> GameState -> Eff _ GameState
update evt state = do
  let evtDir = getDirection evt state.snake.direction
      direction = normaliseDirection evtDir state.snake.direction
      snakeHead = (fromMaybe (fromJust' $ initialState.snake.cells !! 0) (head state.snake.cells))
      eatenFood = fromMaybe { x: 0, y: 0, value: 0, timeout: 999 } (ateFood state.food snakeHead)
      -- eatenSnake = ateSnake state.snake snakeHead
      ind = if (eatenFood.value == 0) then Nothing else (Just 0)
  logShow "hii"
  pure state { snake = changeDirection (moveSnake (growSnake eatenFood.value state.snake)) direction , food = updateFood state.food ind }

ateFood :: FoodState -> SnakeCell -> Maybe FoodCell
ateFood food snakeHead = find (\cell -> cell.x == snakeHead.x && cell.y == snakeHead.y) food.cells

ateSnake :: SnakeState -> SnakeCell -> Maybe SnakeCell
ateSnake snake snakeHead = find (\cell -> cell.x == snakeHead.x && cell.y == snakeHead.y) (fromMaybe (fromJust' $ tail initialState.snake.cells) (tail snake.cells))

updateFood :: FoodState -> Maybe Int -> FoodState
updateFood oldState (Just ind) = oldState { cells = newFood (fromMaybe (oldState.cells) (deleteAt ind oldState.cells)) }
updateFood oldState Nothing = oldState

newFood :: Array FoodCell -> Array FoodCell
newFood foods =
    let newCell = { x: randomInt 0 (gridWidth - 1) , y: randomInt 0 (gridHeight - 1) , value: 1, timeout: randomInt 0 20 }
    in
      newCell : foods

changeDirection :: SnakeState -> Direction -> SnakeState
changeDirection oldSnake dir = oldSnake { direction = dir }

newPosition :: Position -> Direction -> Position
newPosition oldPos Left   = oldPos { x = modulus (oldPos.x - 1) gridWidth  }
newPosition oldPos Right  = oldPos { x = modulus (oldPos.x + 1) gridWidth  }
newPosition oldPos Top    = oldPos { y = modulus (oldPos.y - 1) gridHeight }
newPosition oldPos Bottom = oldPos { y = modulus (oldPos.y + 1) gridHeight }

moveSnake :: SnakeState -> SnakeState
moveSnake oldSnake =
                  let oldSnakeHead = fromMaybe (fromJust' $ initialState.snake.cells !! 0) $ head oldSnake.cells
                      oldSnakeBody = fromMaybe [ { x: 6, y: 6, nextPos: { x: 7, y: 6 } } ] $ tail oldSnake.cells
                      newPos = newPosition oldSnakeHead.nextPos oldSnake.direction
                      newSnakeHead = moveSnakeCell oldSnakeHead newPos
                      newSnakeBody = zipWith (\cell prev -> moveSnakeCell cell ({ x: prev.nextPos.x , y: prev.nextPos.y })) oldSnakeBody oldSnake.cells
                  in
                  oldSnake { cells = newSnakeHead : newSnakeBody }

moveSnakeCell :: SnakeCell -> Position -> SnakeCell
moveSnakeCell oldCell newPos = oldCell { x = oldCell.nextPos.x , y = oldCell.nextPos.y , nextPos = newPos }

growSnake :: Int -> SnakeState -> SnakeState
growSnake value oldSnake = oldSnake { cells = oldSnake.cells <> map (\c -> moveSnakeCell c { x: c.x, y: c.y }) (takeEnd value oldSnake.cells) , size = oldSnake.size + value }

growSnakeUpdate :: Int -> GameState -> GameState
growSnakeUpdate k state = state { snake = growSnake 1 state.snake }

listen = do
  stateE <- E.create
  let stateBehav = id <$> (B.step initialState stateE.event) 
      updateBehav = update <$> keyPress <*> stateBehav
  _ <- B.sample_ stateBehav stateE.event `E.subscribe` (\s -> logShow "hello")
  _ <- stateE.push initialState
  B.sample_ updateBehav (interval 100) `E.subscribe` (\x -> x >>= \s -> stateE.push s >>= \_ -> U.patchAndRun x widget)

-- Helpers
keyPress :: B.Behavior Int
keyPress = map (maybe 37 id) (B.step Nothing (map Just (map (\x -> x.keyCode) move)))

modulus :: Int -> Int -> Int
modulus divd divs = if divd >= 0 then (divd `mod` divs) else (divs - ((divd * -1) `mod` divs))

fromJust' :: forall a. Maybe a -> a
fromJust' = unsafePartial fromJust

evalTest :: GameState -> GameState
evalTest = id

-- Main --
main = do
  U.render (widget initialState) listen
  pure unit