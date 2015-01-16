-- (c) Wilson Berkow
module Game where
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
import Vect (..)
import ArbitraryRounding (arb_round)
import Player (..)
import Fireball (..)
import Platfm (Platfm, configPlatfm, stepPlatfm, renderPlatfm, renderTouchPlatfmPreview)
type WhereTo = Continue Game_State | Pause Game_State | Restart Vect | Die Game_State -- It is necessary for 'Restart' to take a Vect for the same reason it is necessary for DeadScreen.Replay to take one: so that the new game that is started records the previous tap correctly, and does not accidentally pause it or anything
tovect {x,y} = Vect (toFloat x) (toFloat y)
taps_f = Signal.map tovect Touch.taps -- I don't have to fix_origin on the taps, because taps are already automatically in the right coordinate system.
game_background = Collage.filled (Color.rgba 175 175 255 0.75)
                                 (Collage.rect (toFloat game_total_width) (toFloat game_total_height))
-- COMPONENT: Game

type alias Game_State = 

  { plats : List Platfm
  , player : Player
  , fireballs : List Fireball
  , last_touch : Maybe Touch.Touch
  , preview_plat : Maybe Platfm
  , fb_creation_seed : Random.Seed
  , prev_tap_pos : Vect
  , time_playing : Float
  , points : Float
  }
type alias Game_Input = 
 (Maybe Touch.Touch, Vect, Time.Time) -- If and where the player's touching on the screen, and where he/she started the touch (that's part of the data of the Touch type).
cGame_inputs = 
 Signal.map3 (,,) (Signal.map mhead Touch.touches) taps_f (Time.fps framerate)
cGame_step = 

  let touch_to_platfm : Touch.Touch -> Platfm
      touch_to_platfm {x0, y0, x, y} = { start = Vect (toFloat x0) (toFloat y0)
                                       , end = Vect (toFloat x) (toFloat y)
                                       , time_left = 800
                                       }
      point_on_screen : Vect -> Bool
      point_on_screen (Vect x y) = (in_range 0 (toFloat game_total_width) x) && (in_range 0 (toFloat game_total_height) y)
      
      plat_on_screen : Platfm -> Bool
      plat_on_screen {start, end} = point_on_screen start || point_on_screen end
      
      plat_alive : Platfm -> Bool
      plat_alive {time_left} = time_left > 30
      
      fb_on_screen : Fireball -> Bool
      fb_on_screen {pos} = point_on_screen pos || point_on_screen (vect_rise fb_height pos)
      
      player_hitting_fb : Player -> Fireball -> Bool
      player_hitting_fb player fb =
        distance player.pos fb.pos < (configPlayer.radius + fb_height / 2) -- 'fb_height / 2' **approximates** the avg radius of the fb.
      
      update_and_filter dt stepper filterer objs = List.map (stepper dt) (List.filter filterer objs)
      
      step : Game_Input -> Game_State -> WhereTo
      step (cur_touch,cur_tap_pos,dt) g =
        let tap_target = if cur_tap_pos == g.prev_tap_pos then Nothing else Just cur_tap_pos
            pause_clicked =
                case tap_target of
                    Nothing -> False
                    Just (Vect x y) -> x < 50 && y < 50
            restart_clicked =
                case tap_target of
                    Nothing -> False
                    Just (Vect x y) -> x > (toFloat game_total_width - 50) && y < 50
            (should_create_fb, new_fb_pos, new_seed) =
              let (rand_should_create, seed') = Random.generate (Random.int 0 75) g.fb_creation_seed
                  should_create_fb = rand_should_create == 1
                  spacing = round configFireball.padded_len
                  (rand_fb_pos, seed'') = (Random.generate (Random.int 0 (game_total_width // 3)) seed')
                  new_fb_pos = toFloat (arb_round spacing rand_fb_pos) * 3
              in (should_create_fb, new_fb_pos, seed'')
            -- You might notice that there is a '// 3' in the construction of rand_fb_pos,
            --  and a '* 3' in its modification into new_fb_pos. This serves to makes sure
            --  that two fireballs won't be too close, by scaling down the integer-pos-generation
            --  and then expanding it with the '* 3', leaving spaces where fbs might have formed.
            
            drawn_plat = case cur_touch of
                           Nothing -> Maybe.map touch_to_platfm g.last_touch -- If cur_touch is Nothing, that means the user MAY have just released his/her finger.
                           Just _  -> Nothing -- If cur_touch is (Just ...), then the user is still drawing, so nothing should be placed down yet.
            new_plats =
              let updated_plats = update_and_filter dt stepPlatfm (fn_map2 (&&) plat_on_screen plat_alive) g.plats
              in case drawn_plat of Just new_p -> new_p :: updated_plats
                                    Nothing    -> updated_plats
            new_fireballs =
              let updated_fbs = update_and_filter dt stepFireball fb_on_screen g.fireballs
              in if should_create_fb
                  then (makeFireball (Vect new_fb_pos (toFloat game_total_height + fb_height))) :: updated_fbs
                  else updated_fbs
            player_on_fire = any (player_hitting_fb g.player) g.fireballs
            new_game =
              { g | player       <- stepPlayer new_plats g.player
                  , plats        <- new_plats
                  , last_touch   <- cur_touch
                  , fireballs    <- new_fireballs
                  , preview_plat <- Maybe.map touch_to_platfm cur_touch
                  , fb_creation_seed <- new_seed
                  , prev_tap_pos <- cur_tap_pos
                  , time_playing <- g.time_playing + dt
                  , points <- g.points + 2 * (Time.inSeconds dt) * (1 + vect_y g.player.pos / toFloat game_total_height)
                  }
        in if | vect_y g.player.pos > toFloat game_total_height -> Die new_game
              | player_on_fire -> Die new_game
              | pause_clicked -> Pause new_game
              | restart_clicked -> Restart cur_tap_pos
              | otherwise -> Continue new_game
                  
  in step
cGame_render = 

  let btnMargin = 20
      restartBtn = (Collage.toForm (Text.plainText "⟳"))
                     |> move_f (Vect (toFloat game_total_width - btnMargin) btnMargin)
                     |> Collage.scale 2.3
      pauseBtn = Collage.toForm (Text.centered (Text.bold (Text.typeface ["arial", "sans-serif", "monospace"] (Text.fromString "II"))))
                   |> move_f (Vect btnMargin (btnMargin + 2))
                   |> Collage.scale 2
      btn_outline_rad = 65
      btn_outline_clr = Color.rgba 150 150 150 0.25
      btn_outline = (Collage.filled btn_outline_clr (Collage.circle btn_outline_rad))
  in \game ->
    let plats = List.map renderPlatfm game.plats
        plat_preview = Maybe.map renderTouchPlatfmPreview game.preview_plat
        fireballs = List.map renderFireball game.fireballs
        
        forms' = (pauseBtn :: restartBtn :: plats)
              ++ [renderPlayer game.player]
              ++ fireballs
              ++ [ move_f (Vect 10 -5) btn_outline
                 , move_f (Vect (toFloat game_total_width - 10) -5) btn_outline
                 , move_f (Vect (toFloat game_total_width / 2) 20) (Collage.toForm (Text.centered (Text.color Color.black (Text.bold (Text.typeface ["monospace", "arial"] (Text.height 30 (Text.fromString (toString (round game.points)))))))))
                 ]
        forms = case plat_preview of
                  Nothing -> forms'
                  Just p  -> p::forms'
    in Collage.collage game_total_width game_total_height (game_background :: forms)
cGame_init = 

  let side_margin = game_side_margin
      top_margin = game_top_margin
      (gw, gh) = (toFloat game_total_width, toFloat game_total_height)
      (pad_len, side_len) = (configFireball.padded_len, configFireball.side_len)
  in { plats = []
     , player = { pos = Vect 200 75, vel = Vect 0 0 }
     , fireballs = []
     , last_touch = Nothing
     , preview_plat = Nothing
     , fb_creation_seed = Random.initialSeed 1234567890987654321 -- CHANGE THIS TO MAKE IT VARY
     , prev_tap_pos = Vect 0 0
     , time_playing = 0
     , points = 0
     }