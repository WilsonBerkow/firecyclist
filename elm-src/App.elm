-- (c) Wilson Berkow

module App (State, init, step, render, Input) where

import Text (plainText)
import Signal ((<~))
import Time (Time)

import Config (game_total_height)
import Game
import Paused
import DeadScreen
import MainMenu

type State = OnGame Game.State | OnDead Game.State | OnPaused Game.State | OnMainMenu MainMenu.State
type alias Input = (Time, Game.Input) -- The Time is the current time, from port approx_time

sndOfThree (_,x,_) = x

step (t, inputs) st =
  case st of
    (OnGame g) ->
      case Game.step inputs g of
        Game.Die g -> OnDead g
        Game.Restart pos -> let i = (Game.init t) in OnGame { i | prev_tap_pos <- pos }
        Game.Pause st -> OnPaused st
        Game.Continue new_game -> OnGame new_game
    (OnPaused g) ->
      case Paused.step (sndOfThree inputs) g of
        Paused.Continue g -> OnPaused g
        Paused.Play g -> OnGame g
    (OnDead g) ->
      case DeadScreen.step (sndOfThree inputs) g of
        DeadScreen.Continue g -> OnDead g
        DeadScreen.Replay pos -> let i = (Game.init t) in OnGame { i | prev_tap_pos <- pos }
    (OnMainMenu p) ->
      case MainMenu.step (sndOfThree inputs) p of
        MainMenu.Continue pos -> OnMainMenu pos
        MainMenu.PlayGame -> OnGame (Game.init t)

render st =
  case st of
    (OnGame g) -> Game.render g
    (OnPaused g) -> Paused.render g
    (OnDead g) -> DeadScreen.render g
    (OnMainMenu p) -> MainMenu.render p

init = OnMainMenu MainMenu.init

