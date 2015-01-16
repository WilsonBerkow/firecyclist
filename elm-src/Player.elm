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
      
      intersects : Player -> Platfm -> Bool
      intersects player plat = -- This algorithm is from http://mathworld.wolfram.com/Circle-LineIntersection.html
        in_range plat.start.x plat.end.x player.pos.x && in_range plat.start.y plat.end.y player.pos.y &&
          (let (px, py) = let {x,y} = player.pos in (x,y)
               r = configPlayer.radius
               platlength = distance plat.start plat.end
               (offsetx0, offsety0) = let {x,y} = vect_subtract plat.start player.pos in (x,y)
               (offsetx1, offsety1) = let {x,y} = vect_subtract plat.end player.pos in (x,y)
               big_D = offsetx0 * offsety1 - offsetx1 * offsety0
           in (r * platlength) ^ 2 >= big_D ^ 2)
      
      touching_any : Player -> List Platfm -> Maybe Platfm
      touching_any pl plats =
        case plats of
          p::ps ->
            if intersects pl p
              then Just p
              else touching_any pl ps
          [] -> Nothing
      
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
                         (newx, newy) = let {x,y} = vect_add p.pos vel in (x,y)
                         gwidth = toFloat game_total_width
                         rad = configPlayer.radius
                         modded_newx = if | newx > gwidth + rad -> newx - gwidth - rad
                                          | newx < -rad         -> newx + gwidth + rad
                                          | otherwise           -> newx
                     in { pos = {x=modded_newx,y=newy}
                        , vel = vel
                        }
  in step