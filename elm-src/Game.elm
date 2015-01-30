-- (c) Wilson Berkow

module Game (game_background, State, Input, init, render, step, WhereTo(Continue,Pause,Restart,Die)) where

import List
import List ((::))
import Signal
import Signal (Signal)
import Touch
import Random
import Maybe
import Text
import Time
import Color
import Graphics.Collage as Collage
import Graphics.Element (Element)

import Config (game_side_margin, game_top_margin, game_total_width, game_total_height, framerate)
import BasicUtil (..)
import HasPosition (..)
import ArbitraryRounding (arb_round)
import Player (..)
import Fireball (..)
import Platfm (Platfm, configPlatfm, stepPlatfm, renderPlatfm, renderTouchPlatfmPreview)
import Coin (..)
import Target (..)

type WhereTo = Continue State | Pause State | Restart Position | Die State -- It is necessary for 'Restart' to take a Position for the same reason it is necessary for DeadScreen.Replay to take one: so that the new game that is started records the previous tap correctly, and does not accidentally pause it or anything
toPosition {x,y} = { x = (toFloat x), y = (toFloat y) }

game_background = Collage.filled (Color.rgba 175 175 255 0.75)
                                 (Collage.rect (toFloat game_total_width) (toFloat game_total_height))

type alias State =
  { plats : List Platfm
  , player : Player
  , coins : List Coin
  , targets: List Target
  , fireballs : List Fireball
  , last_touch : Maybe Touch.Touch
  , preview_plat : Maybe Platfm
  , t0_preview_plat_just_added : Maybe Time.Time -- Remember the t0 (used to identify platfms) of the platform that was most recently drawn on the screen as a result of it being hit by the player. This is used to make sure that that platform does not get drawn again.
  , fb_creation_seed : Random.Seed
  , prev_tap_pos : Position
  , time_playing : Float
  , points : Float
  , just_a_simulation : Bool
  }
type alias Input = (Maybe Touch.Touch, Position, Time.Time) -- If and where the player's touching on the screen, and where he/she started the touch (that's part of the data of the Touch type).

step : Input -> State -> WhereTo
step =
  let touch_to_platfm : Touch.Touch -> Platfm
      touch_to_platfm {x0, y0, x, y} = { start = { x = toFloat x0, y = toFloat y0 }
                                       , end = { x = toFloat x, y = toFloat y }
                                       , time_left = 800
                                       }
      point_on_screen : HasPosition r -> Bool
      point_on_screen {x,y} = in_range 0 (toFloat game_total_width) x && in_range 0 (toFloat game_total_height) y
      
      plat_on_screen : Platfm -> Bool
      plat_on_screen {start, end} = point_on_screen start || point_on_screen end
      
      plat_alive : Platfm -> Bool
      plat_alive {time_left} = time_left > 30
      
      plat_should_stay : Platfm -> Bool
      plat_should_stay = fn_map2 (&&) plat_on_screen plat_alive
      
      fb_on_screen : Fireball -> Bool
      fb_on_screen {pos} = point_on_screen pos || point_on_screen (vect_rise fb_height pos)
      
      player_hitting_fb : Player -> Fireball -> Bool
      player_hitting_fb player fb =
        distance player.pos fb.pos < (configPlayer.radius + fb_radius)
      
      coin_on_screen : Coin -> Bool
      coin_on_screen pos = point_on_screen pos || point_on_screen (vect_rise coin_radius pos)
      
      coin_hitting_player : Player -> Coin -> Bool
      coin_hitting_player pl coin = distance pl.pos coin < (configPlayer.radius + coin_radius)
      
      target_alive : Target -> Bool
      target_alive (_, lifespan, _) = lifespan >= 0
      
      target_hitting_player : Player -> Target -> Bool
      target_hitting_player pl (pos, lifeleft, _) = distance pl.pos pos < (configPlayer.radius + target_radius * (lifeleft / target_lifespan))
      
      update_and_filter stepper filterer objs = List.map stepper (List.filter filterer objs)
      
      randomly_create_x seed dt likelihood spacing =
        let (rand_should_create, seed') = Random.generate (Random.float 0 (100 / (dt / 30) / likelihood)) seed
            should_create_fb = rand_should_create < 1
            (rand_fb_pos, seed'') = (Random.generate (Random.int 0 (game_total_width // 3)) seed')
            new_fb_pos =
              if should_create_fb
                then Just (toFloat (arb_round spacing rand_fb_pos) * 3)
                else Nothing
        in (new_fb_pos, seed'')
        -- You might notice that there is a '// 3' in the construction of rand_fb_pos,
        --  and a '* 3' in its modification into new_fb_pos. This serves to makes sure
        --  that two fireballs won't be too close, by scaling down the integer-pos-generation
        --  and then expanding it with the '* 3', leaving spaces where fbs might have formed.
      
      step : Input -> State -> WhereTo
      step (cur_touch,cur_tap_pos,dt) g =
        let tap_target = if cur_tap_pos == g.prev_tap_pos then Nothing else Just cur_tap_pos
            pause_clicked =
              case tap_target of
                Nothing -> False
                Just {x, y} -> x < 50 && y < 50
            restart_clicked =
              case tap_target of
                Nothing -> False
                Just {x, y} -> x > (toFloat game_total_width - 50) && y < 50
            (new_fb_pos, seed') = randomly_create_x g.fb_creation_seed dt 1 (round configFireball.padded_len)
            (new_coin_pos, seed'') = randomly_create_x seed' dt 0.4 (round coin_radius)
            (new_target_x, seed''') = randomly_create_x seed'' dt 0.2 (round target_radius)
            (random_target_y, new_seed) = Random.generate (Random.float 25 (toFloat game_total_height * 1 / 3 + 25)) seed'''
            confirm_platfm_validity ({start, end} as plat) =
              if | start == end -> Nothing
                 | start.y == end.y -> Just { plat | start <- { start | y <- plat.start.y - 1 } } -- This handles when the platform is drawn perfectly vertically.
                 | otherwise -> Just plat
            
            new_preview_plat : Maybe Platfm
            new_preview_plat = -- This does a check to make sure that the 'preview plat' has NOT already been drawn/materialized by having been landed on by the player.
              case (g.t0_preview_plat_just_added, cur_touch) of
                (Just prev_t0, Just cur) ->
                  if prev_t0 == cur.t0
                    then Nothing
                    else Just (touch_to_platfm cur) `Maybe.andThen` confirm_platfm_validity
                (Nothing, Just cur) -> Just (touch_to_platfm cur) `Maybe.andThen` confirm_platfm_validity
                (_, Nothing) -> Nothing
            
            drawn_plat : Maybe Platfm
            drawn_plat =
              case cur_touch of
                Nothing -> -- If cur_touch is Nothing, that means the user MAY have just released his/her finger.
                  maybeOr new_preview_plat g.preview_plat `Maybe.andThen` confirm_platfm_validity
                Just _  ->
                 Nothing -- If cur_touch is (Just ...), then the user is still drawing, so nothing should be placed down yet.
            
            should_add_preview_plat : Maybe Platfm
            should_add_preview_plat =
              Maybe.andThen new_preview_plat
                (\plat ->
                   if intersects_plat g.player plat
                     then Just plat
                     else Nothing)
            
            new_plats : List Platfm
            new_plats =
              let updated_plats = update_and_filter (stepPlatfm dt) plat_should_stay g.plats
              in case (should_add_preview_plat, drawn_plat) of
                   (_, Just new_p) -> new_p :: updated_plats
                   (Just new_p, _) -> new_p :: updated_plats
                   _               -> updated_plats
            
            new_fireballs : List Fireball
            new_fireballs =
              let updated_fbs = update_and_filter (stepFireball dt) fb_on_screen g.fireballs
              in case new_fb_pos of
                   Just xpos -> makeFireball { x = xpos, y = toFloat game_total_height + fb_height } :: updated_fbs
                   Nothing -> updated_fbs
            player_on_fire = any (player_hitting_fb g.player) g.fireballs
            
            points_from_coins = 5 * toFloat (List.length (List.filter (coin_hitting_player g.player) g.coins))
            
            new_coins : List Coin
            new_coins =
              let updated_coins = update_and_filter (stepCoin dt) (fn_map2 (&&) coin_on_screen (not << coin_hitting_player g.player)) g.coins
              in case new_coin_pos of
                   Just xpos -> { x = xpos, y = toFloat game_total_height + coin_radius } :: updated_coins
                   Nothing -> updated_coins
            
            new_targets : List Target
            new_targets =
              let updated_targets = update_and_filter (stepTarget dt) (fn_map2 (&&) target_alive (not << target_hitting_player g.player)) g.targets -- Testing whether it's been hit is a temporary measure. Later it will be whether the hit-animation has finished.
              in case new_target_x of
                   Just xpos -> ({ x = xpos, y = random_target_y }, target_lifespan, Nothing) :: updated_targets -- The hard-coded y coordinate is probably just for now.
                   Nothing -> updated_targets
            
            new_game : State
            new_game =
              { player           = stepPlayer (new_plats,dt) g.player
              , plats            = new_plats
              , coins            = new_coins
              , targets          = new_targets
              , last_touch       = cur_touch
              , fireballs        = new_fireballs
              , preview_plat     = if isJust should_add_preview_plat then Nothing else new_preview_plat
              , t0_preview_plat_just_added =
                  if isJust should_add_preview_plat
                    then Maybe.map .t0 cur_touch
                    else g.t0_preview_plat_just_added
              , fb_creation_seed = new_seed
              , prev_tap_pos     = cur_tap_pos
              , time_playing     = g.time_playing + dt
              , points           = g.points + 2 * (Time.inSeconds dt) * (1 + g.player.pos.y / toFloat game_total_height) + points_from_coins
              , just_a_simulation = False
              }
        in if | g.player.pos.y > toFloat game_total_height -> Die new_game
              | player_on_fire -> Die new_game
              | pause_clicked -> Pause new_game
              | restart_clicked -> Restart cur_tap_pos
              | otherwise -> Continue new_game
  in step

render : State -> Element
render =
  let btnMargin = 20
      restartBtn = (Collage.toForm (Text.plainText "R"))--"&#10227;"))
                     |> move_f { x = toFloat game_total_width - btnMargin, y = btnMargin }
                     |> Collage.scale 2.3
      pauseBtn = Collage.toForm (Text.centered (Text.bold (Text.typeface ["arial", "sans-serif", "monospace"] (Text.fromString "II"))))
                   |> move_f { x = btnMargin, y = btnMargin + 2 }
                   |> Collage.scale 2
      btn_outline_rad = 65
      btn_outline_clr = Color.rgba 150 150 150 0.25
      btn_outline = (Collage.filled btn_outline_clr (Collage.circle btn_outline_rad))
  in \game ->
    let plats = List.map renderPlatfm game.plats
        coins = List.map renderCoin game.coins
        targets = List.map renderTarget game.targets
        plat_preview = Maybe.map renderTouchPlatfmPreview game.preview_plat
        fireballs = List.map renderFireball game.fireballs
        
        forms' = [pauseBtn, restartBtn]
              ++ [ move_f {x=10, y=-5} btn_outline
                 , move_f {x=toFloat game_total_width - 10, y=-5} btn_outline
                 , move_f {x=toFloat game_total_width / 2, y=20} (Collage.toForm (Text.centered (Text.color Color.black (Text.bold (Text.typeface ["monospace", "arial"] (Text.height 30 (Text.fromString (toString (round game.points)))))))))
                 ]
              ++ plats
              ++ targets
              ++ coins
              ++ fireballs
              ++ [renderPlayer game.player]
        forms = case plat_preview of
                  Nothing -> forms'
                  Just p  -> p::forms'
    in Collage.collage game_total_width game_total_height (game_background :: forms)

init : Time.Time -> State
init current_time =
  let side_margin = game_side_margin
      top_margin = game_top_margin
      (gw, gh) = (toFloat game_total_width, toFloat game_total_height)
      (pad_len, side_len) = (configFireball.padded_len, configFireball.side_len)
  in { plats = []
     , player = { pos = {x=200,y=75}, vel = {x=0,y=0} }
     , coins = []
     , targets = []
     , fireballs = []
     , last_touch = Nothing
     , preview_plat = Nothing
     , t0_preview_plat_just_added = Nothing -- TODO: change this to hold the .id of the plat
     , fb_creation_seed = Random.initialSeed (round current_time)
     , prev_tap_pos = {x=0,y=0}
     , time_playing = 0
     , points = 0
     , just_a_simulation = False
     }