-- (c) Wilson Berkow.

module DeadScreen (WhereTo(Continue, Replay), State, Input, step, render) where

import Text
import Time
import Graphics.Element as Element
import Graphics.Collage (collage, moveY, rect, toForm, filled, group)
import Color

import Config (game_total_width, game_total_height)
import HasPosition (Position)
import Game

type WhereTo = Continue State | Replay Position

type alias State = Game.State
type alias Input = Position

step tap_pos g =
  let new_g = { g | prev_tap_pos <- tap_pos }
  in if tap_pos /= g.prev_tap_pos
       then Replay tap_pos
       else Continue new_g

messageStyle = Text.bold << Text.typeface ["monospace", "arial"]

render g =
    collage game_total_width game_total_height
      [ toForm (Game.render g)
      , filled (Color.rgba 200 200 200 0.5) (rect (toFloat game_total_width) (toFloat game_total_height))
      , moveY  80 <| toForm <| Text.centered (Text.height  80 (Text.color Color.orange (Text.italic (messageStyle (Text.fromString "Game\nOver\n\n")))))
      , moveY -30 <| toForm <| Text.centered (Text.height 150 (Text.color Color.orange (messageStyle (Text.fromString (toString (round g.points))))))
      ]