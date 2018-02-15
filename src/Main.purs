module Main where

import Prelude
import Types
import UI.Elements
import UI.Events
import UI.Properties
import Math

import Data.Maybe
import FRP.Event
import FRP.Event.Time (animationFrame, interval)
import FRP.Behavior as B
import FRP.Behavior.Keyboard (keys)
import Data.Int (fromNumber, toNumber)
import FRP.Event.Class
import Halogen.VDom
import Data.Array
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Plus ((<|>))
import UI.Core (MEvent, AttrValue(..), Attr(..), Prop)
import UI.Util as U

import FRP.Behavior.Keyboard (keys, key)

foreign import click :: MEvent
foreign import move :: Event { keyCode :: Int }

----------------- Global Constants -------------------

-- Box -- ViewBox Size in actual pixels
boxHeight :: Number
boxHeight = 500.0

boxWidth :: Number
boxWidth = 800.0

-- Grid -- Snake Grid 
gridHeight :: Int
gridHeight = 50

gridWidth :: Int
gridWidth = 80

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

type FoodCell = { x :: Int , y :: Int , value :: Int , timeout :: Int }

type SnakeCell = { x :: Int , y :: Int , direction :: Direction } 

data Cell = FC FoodCell | SC SnakeCell

type SnakeState = { cells :: Array SnakeCell , size :: Int , direction :: Direction , prevDirection :: Direction }

type FoodState = { cells :: Array FoodCell , count :: Int }

type State = { game :: { snake :: SnakeState , food :: FoodState } }

initialState :: State
initialState = { game: { snake : { cells: [ { x: 5 , y: 5 , direction: Right }
                                          ]
                                  , size: 1
                                  , direction: Right
                                  , prevDirection: Right
                                  }
                        , food : { cells: [ { x: 10 , y: 20 , value: 1 , timeout: 10 } ]
                                 , count: 1
                                 }
                        }
                }
              
-- View --

toMargin :: Int -> Int -> String
toMargin x y = (show $ (toNumber x) * cellWidth) <> "," <> (show $ (toNumber y) * cellHeight) <> ",0,0"

widget :: forall i. State -> VDom Attr i
widget state = linearLayout
               [ id_ "app"
               , height "match_parent"
               , width "match_parent"
               , background "#f0f0f0"
               , gravity "center"
               ]
               [ relativeLayout
                 [ id_ "game"
                 , height (show boxHeight)
                 , width (show boxWidth)
                 , background "#ffffff"
                 ] [ snake state.game.snake, food state.game.food ]
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
                        , height (show cellHeight)
                        , width (show cellWidth)
                        , background "#484848"
                        , margin (toMargin state.x state.y)
                        ] []

foodCell :: forall i. Int -> FoodCell -> VDom Attr i
foodCell index state = linearLayout
                       [ id_ ("food" <> (show index))
                       , height (show cellHeight)
                       , width (show cellWidth)
                       , background "#e84118"
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

update :: Int -> Eff _ State
update evt = do
  state <- U.getState
  let currDirection = getDirection evt state.game.snake.direction
  let newState = state { game { snake = moveSnake state.game.snake currDirection } }
  _ <- U.updateState "game" newState.game
  pure newState

keyPress :: B.Behavior Int
keyPress = map (maybe 37 id) (B.step Nothing (map Just (map (\x -> x.keyCode) move)))

modulus :: Int -> Int -> Int
modulus divd divs = if divd >= 0 then (divd `mod` divs) else (divs - ((divd * -1) `mod` divs))

moveSnake :: SnakeState -> Direction -> SnakeState
moveSnake oldSnake dir = oldSnake { cells = (zipWith moveSnakeCell oldSnake.cells (dir : map _.direction oldSnake.cells)) , direction = dir }

moveSnakeCell :: SnakeCell -> Direction -> SnakeCell
moveSnakeCell oldCell Left   = oldCell { x = modulus (oldCell.x - 1) gridWidth  , direction = Left   }
moveSnakeCell oldCell Right  = oldCell { x = modulus (oldCell.x + 1) gridWidth  , direction = Right  }
moveSnakeCell oldCell Top    = oldCell { y = modulus (oldCell.y - 1) gridHeight , direction = Top    }
moveSnakeCell oldCell Bottom = oldCell { y = modulus (oldCell.y + 1) gridHeight , direction = Bottom }

growSnake :: SnakeState -> Int -> SnakeState
growSnake oldSnake points = oldSnake { cells = oldSnake.cells <> map (\c -> moveSnakeCell c c.direction) (takeEnd points oldSnake.cells)  }

listen = do
  let behavior = update <$> keyPress
  U.patch widget behavior (sampleOn_ animationFrame (interval 20))

-- Main --
main = do
  U.initializeState
  state <- U.updateState "game" initialState.game
  U.render (widget state) listen
  pure unit