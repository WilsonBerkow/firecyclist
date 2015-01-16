-- (c) Wilson Berkow
module App (App_State, cApp_init, cApp_step, cApp_render, App_Input, cApp_inputs) where
import Text (plainText)
import Config (game_total_height)
import Game (..)
import Paused
import Paused (cPaused_step, cPaused_render)
import DeadScreen
import DeadScreen (cDeadScreen_step, cDeadScreen_render)
import MainMenu
import MainMenu (MainMenu_State, cMainMenu_init, cMainMenu_step, cMainMenu_render)
type St = OnGame Game_State | OnDead Game_State | OnPaused Game_State | OnMainMenu MainMenu_State
-- COMPONENT: App
cApp_localvar_sndOfThree = \(_,x,_) -> x

type alias App_State = 
 St
type alias App_Input = 
 Game_Input
cApp_inputs = 
 cGame_inputs
cApp_step : App_Input -> St -> St
cApp_step = 
 \inputs st ->
  case st of
    (OnGame g) ->
      case cGame_step inputs g of
        Die g -> OnDead g
        Restart pos -> OnGame { cGame_init | prev_tap_pos <- pos }
        Pause st -> OnPaused st
        Continue new_game -> OnGame new_game
    (OnPaused g) ->
      case cPaused_step (cApp_localvar_sndOfThree inputs) g of
        Paused.Continue g -> OnPaused g
        Paused.Play g -> OnGame g
    (OnDead g) ->
      case cDeadScreen_step (cApp_localvar_sndOfThree inputs) g of
        DeadScreen.Continue g -> OnDead g
        DeadScreen.Replay pos -> OnGame { cGame_init | prev_tap_pos <- pos }
    (OnMainMenu p) ->
      case cMainMenu_step (cApp_localvar_sndOfThree inputs) p of
        MainMenu.Continue pos -> OnMainMenu pos
        MainMenu.PlayGame -> OnGame cGame_init
cApp_render = 
 \st ->
    case st of
        (OnGame g) -> cGame_render g
        (OnPaused g) -> cPaused_render g
        (OnDead s) -> cDeadScreen_render s
        (OnMainMenu p) -> cMainMenu_render p
cApp_init = 
 OnMainMenu cMainMenu_init