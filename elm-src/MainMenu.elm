-- (c) Wilson Berkow.
module MainMenu where
import Text
import Time
import Graphics.Element as Element
import Graphics.Collage (collage, moveY, rect, toForm, filled, group)
import Color
import Config (game_total_width, game_total_height)
import Game (Game_State, cGame_render, taps_f, game_background)
import HasPosition (Position)
import BasicUtil (deepGrey)
type WhereTo = Continue MainMenu_State | PlayGame
-- COMPONENT: MainMenu
cMainMenu_localvar_messageStyle = Text.color Color.darkGrey << Text.bold << Text.height 40 << cMainMenu_localvar_monospace
cMainMenu_localvar_monospace = Text.typeface ["monospace", "arial"]

type alias MainMenu_State = 
 Position -- the prev pos
type alias MainMenu_Input = 
 Position
cMainMenu_inputs = 
 taps_f
cMainMenu_step = 
 \tap_pos prev_tap_pos ->
  if tap_pos /= prev_tap_pos
    then PlayGame
    else Continue tap_pos
cMainMenu_render = 
 \_ -> 
    collage game_total_width game_total_height
      [ game_background
      , filled (Color.rgba 200 200 200 0.5) (rect (toFloat game_total_width) (toFloat game_total_height))
      , moveY 40 <| group
          [ moveY   70 <| toForm <| Text.centered (Text.bold (Text.color Color.orange   (Text.height 140 (cMainMenu_localvar_monospace (Text.fromString "Fire")))))
          , moveY    0 <| toForm <| Text.centered (Text.bold (Text.color Color.orange   (Text.height  85 (cMainMenu_localvar_monospace (Text.fromString "cyclist")))))
          , moveY -100 <| toForm <| Text.centered (Text.bold (Text.color deepGrey       (Text.height  60 (Text.italic (cMainMenu_localvar_monospace (Text.fromString "Play"))))))
          ]
      ]
cMainMenu_init = 
 {x=0,y=0}
  