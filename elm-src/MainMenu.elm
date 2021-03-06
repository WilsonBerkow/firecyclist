-- (c) Wilson Berkow.

module MainMenu (WhereTo(Continue, PlayGame), State, Input, step, render, init) where

import Text
import Time
import Graphics.Element as Element
import Graphics.Collage (collage, moveY, rect, toForm, filled, group)
import Color

import Config (game_total_width, game_total_height)
import Game
import HasPosition (Position)
import BasicUtil (deepGrey)

type WhereTo = Continue State | PlayGame
type alias State = Position -- the position previously reported by the 'taps' input
type alias Input = Position

step tap_pos prev_tap_pos =
  if tap_pos /= prev_tap_pos
    then PlayGame
    else Continue tap_pos
messageStyle = Text.color Color.darkGrey << Text.bold << Text.height 40 << monospace
monospace = Text.typeface ["monospace", "arial"]
arial = Text.typeface ["arial", "monospace"]
render _ = 
  collage game_total_width game_total_height
    [ Game.game_background
    , filled (Color.rgba 200 200 200 0.5) (rect (toFloat game_total_width) (toFloat game_total_height))
    , moveY 40 <| group
        [ moveY   75 <| toForm <| Text.centered (Text.bold (Text.italic (Text.color Color.orange (Text.height 165 (arial (Text.fromString "Fire"))))))
        , moveY    0 <| toForm <| Text.centered (Text.bold (Text.italic (Text.color Color.orange (Text.height  95 (arial (Text.fromString "cyclist"))))))
        , moveY -100 <| toForm <| Text.centered (Text.bold (Text.color deepGrey                  (Text.height  60 (identity (monospace (Text.fromString "Play"))))))
        ]
    ]
init = {x=0,y=0}
  