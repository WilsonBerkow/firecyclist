-- (c) Wilson Berkow.
module DeadScreen where
import Text
import Time
import Graphics.Element as Element
import Graphics.Collage (collage, moveY, rect, toForm, filled, group)
import Color
import Config (game_total_width, game_total_height)
import Vect (Vect, vect_x)
import Game (Game_State, cGame_render, taps_f)
type WhereTo = Continue DeadScreen_State | Replay Vect
-- COMPONENT: DeadScreen
cDeadScreen_localvar_messageStyle = Text.bold << Text.typeface ["monospace", "arial"]

type alias DeadScreen_State = 
 Game_State
type alias DeadScreen_Input = 
 Vect
cDeadScreen_inputs = 
 taps_f
cDeadScreen_step = 
 \tap_pos g ->
    let new_g = { g | prev_tap_pos <- tap_pos }
    in if tap_pos /= g.prev_tap_pos
         then Replay tap_pos
         else Continue new_g
cDeadScreen_render = 
 \g -> 
    collage game_total_width game_total_height
      [ toForm (cGame_render g)
      , filled (Color.rgba 200 200 200 0.5) (rect (toFloat game_total_width) (toFloat game_total_height))
      , moveY  80 <| toForm <| Text.centered (Text.height  80 (Text.color Color.orange (Text.italic (cDeadScreen_localvar_messageStyle (Text.fromString "Game\nOver\n\n")))))
      , moveY -30 <| toForm <| Text.centered (Text.height 150 (Text.color Color.orange (cDeadScreen_localvar_messageStyle (Text.fromString (toString (round (Time.inSeconds g.time_playing)))))))
      ]