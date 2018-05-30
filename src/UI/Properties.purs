module UI.Properties where

import Prelude

import Data.Tuple (Tuple(..))

import UI.Core (AttrValue(..), Prop)


prop :: String -> String -> Prop
prop key value = Tuple key (AttrValue value)

id_ :: String -> Prop
id_ = prop "id"

height :: String -> Prop
height = prop "height"

width :: String -> Prop
width = prop "width"

background :: String -> Prop
background = prop "background"

margin :: String -> Prop
margin = prop "margin"

color :: String -> Prop
color = prop "color"

text :: String -> Prop
text = prop "text"

bg :: String -> Prop
bg = prop "bg"

gravity :: String -> Prop
gravity = prop "gravity"

stroke :: String -> Prop
stroke = prop "stroke"

cornerRadius :: String -> Prop
cornerRadius = prop "cornerRadius"

className :: String -> Prop
className = prop "className"