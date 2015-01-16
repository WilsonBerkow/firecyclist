-- (c) Wilson Berkow

module Player where

import List
import Graphics.Collage as Collage
import Color

import Config (game_side_margin, game_top_margin, game_total_width, game_total_height)
import BasicUtil (..)
import Vect (..)
import Platfm (Platfm, configPlatfm)

type alias Player = { pos : Vect, vel : Vect }
type alias PlayerInputs = List Platfm

std_player =
  let blue_gray = Color.rgba 50 50 200 0.7
      secondary = Color.rgba 50 50 200 1
  in Collage.group [ Collage.filled blue_gray (Collage.circle configPlayer.radius)
                   , Collage.outlined (Collage.solid secondary) (Collage.circle configPlayer.radius)
                   ]

renderPlayer : Player -> Collage.Form
renderPlayer p = move_f p.pos std_player

configPlayer = { radius = 10 }

stepPlayer : PlayerInputs -> Player -> Player
stepPlayer =
  let slope (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1)
      
      -- point-slope form: y = m(x - x1) + y1
      line_to_func ((x1, y1) as a) b = let m = slope a b
                                       in \x -> m * (x - x1) + y1
      
      touching : (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
      touching a b = let fn = line_to_func a b
                         in_r = in_range (fst a) (fst b)
                     in \(pl_x, pl_y) -> (in_r pl_x) && ((abs <| (fn pl_x) - pl_y) <= (2*configPlayer.radius))
      
      touching_plat : Player -> Platfm -> Bool
      touching_plat {pos} {start, end} = let (Vect x1 y1) = start
                                             (Vect x2 y2) = end
                                             (Vect pl_x pl_y) = pos
                                         in touching (x1, y1) (x2, y2) (pl_x, pl_y)
      
      touching_any : Player -> List Platfm -> Maybe Platfm
      touching_any pl plats = List.foldl (\plat acc -> if | isJust acc            -> acc -- If a collision has already been found (i.e., if acc is (Just ...)), just keep returning it.
                                                          | touching_plat pl plat -> Just plat
                                                          | otherwise             -> acc)
                                         Nothing
                                         plats
      
      plat_slope : Platfm -> Float
      plat_slope {start, end} = let (Vect x1 y1) = start
                                    (Vect x2 y2) = end
                                in slope (x1, y1) (x2, y2)
      platfm_bounciness = 0.75--1--.5--3 or 0, with lower sliding speed (vscale below)
      vel_from_slope m = Vect (signnum m) (abs m)
                          |> vscale 3--6
                          |> vect_rise configPlatfm.fall_rate
                          |> vect_rise platfm_bounciness
      player_grav = 0.2--0.3
      
      step plats p = let on_plat = touching_any p plats
                         vel = case on_plat of
                                 Just plat -> vel_from_slope (plat_slope plat)
                                 Nothing -> vect_fall player_grav p.vel
                         (Vect newx newy) = vsum [p.pos, vel]
                         gwidth = toFloat game_total_width
                         rad = configPlayer.radius
                         modded_newx = if | newx > gwidth + rad -> newx - gwidth - rad
                                          | newx < -rad         -> newx + gwidth + rad
                                          | otherwise           -> newx
                     in { pos = Vect modded_newx newy
                        , vel = vel
                        }
  in step