-- (c) Wilson Berkow

module Player where

import List
import Graphics.Collage as Collage
import Color

import Config (game_side_margin, game_top_margin, game_total_width, game_total_height)
import BasicUtil (..)
import HasPosition (..)
import Platfm (Platfm, configPlatfm)

type alias Player = { pos : Position, vel : Position }
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
      
      intersects : Player -> Platfm -> Bool
      intersects player plat = -- This algorithm is from http://mathworld.wolfram.com/Circle-LineIntersection.html
        let (px, py) = let {x,y} = player.pos in (x,y)
            r = configPlayer.radius
            platlength = distance plat.start plat.end
            (offsetx0, offsety0) = let {x,y} = vect_subtract plat.start player.pos in (x,y)
            (offsetx1, offsety1) = let {x,y} = vect_subtract plat.end player.pos in (x,y)
            big_D = offsetx0 * offsety1 - offsetx1 * offsety0
        in (r * platlength) ^ 2 >= big_D ^ 2
      
      touching_plat : Player -> Platfm -> Bool
      touching_plat {pos} {start, end} = let (x1, y1) = let {x,y} = start in (x,y)
                                             (x2, y2) = let {x,y} = end in (x,y)
                                             (pl_x, pl_y) = let {x,y} = pos in (x,y)
                                         in touching (x1, y1) (x2, y2) (pl_x, pl_y)
      
      touching_any : Player -> List Platfm -> Maybe Platfm
      touching_any pl plats = List.foldl (\plat acc -> if | isJust acc         -> acc -- If a collision has already been found (i.e., if acc is (Just ...)), just keep returning it.
                                                          | intersects pl plat -> Just plat
                                                          | otherwise          -> acc)
                                         Nothing
                                         plats
      
      plat_slope : Platfm -> Float
      plat_slope {start, end} = slope (start.x, start.y) (end.x, end.y)
      platfm_bounciness = 0.75--1--.5--3 or 0, with lower sliding speed (vscale below)
      vel_from_slope m = { x = signnum m, y = abs m }
                          |> vscale 3--6
                          |> vect_rise configPlatfm.fall_rate
                          |> vect_rise platfm_bounciness
      player_grav = 0.2--0.3
      
      step plats p = let on_plat = touching_any p plats
                         vel = case on_plat of
                                 Just plat -> vel_from_slope (plat_slope plat)
                                 Nothing -> vect_fall player_grav p.vel
                         (newx, newy) = let {x,y} = vsum [p.pos, vel] in (x,y)
                         gwidth = toFloat game_total_width
                         rad = configPlayer.radius
                         modded_newx = if | newx > gwidth + rad -> newx - gwidth - rad
                                          | newx < -rad         -> newx + gwidth + rad
                                          | otherwise           -> newx
                     in { pos = {x=modded_newx,y=newy}
                        , vel = vel
                        }
  in step