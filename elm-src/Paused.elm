-- (c) Wilson Berkow
module Paused (Paused_State, Paused_Input, cPaused_step, cPaused_render, cPaused_inputs, WhereTo(Continue,Play)) where
-- NOTE: It would probably be better to just make Game_State a sum type of (Playing ...) and (Paused ...), or have
--  some .paused property, and then put all this in there. Also, for this to really be well-designed, there should
--  be a component in between App and Game, perhaps rename Game GamePlay and have a Game component which basically
--  just handles the GamePlay screen, Paused screen, and DeadScreen screen, because all of those use the Game_State
--  for that round of the game. If I do that, then maybe I would have that local-GlobalState that I was thinking
--  about before, with what is now Game_State being Game,GamePlay,Paused,Dead_GlobalState
import Text
import Graphics.Element as Element
import Graphics.Collage as Collage
import Color
import Config (game_total_width, game_total_height)
import HasPosition (Position)
import Game (Game_State, cGame_render, taps_f)
type WhereTo = Continue Paused_State | Play Paused_State
-- COMPONENT: Paused
cPaused_localvar_messageStyle = Text.color Color.orange << Text.bold << Text.height 60 << Text.typeface ["monospace", "arial"]

type alias Paused_State = 
 Game_State
type alias Paused_Input = 
 Position
cPaused_inputs = 
 taps_f
cPaused_step = 
 \tap_pos g ->
    let new_g = { g | prev_tap_pos <- tap_pos }
    in if tap_pos /= g.prev_tap_pos
         then Play new_g
         else Continue new_g
cPaused_render = 
 \g -> 
    Collage.collage game_total_width game_total_height
      [ Collage.toForm (cGame_render g)
      , Collage.filled (Color.rgba 200 200 200 0.5) (Collage.rect (toFloat game_total_width) (toFloat game_total_height))
      , Collage.moveY 20 (Collage.toForm (Text.centered (cPaused_localvar_messageStyle (Text.fromString "Paused"))))
      ]