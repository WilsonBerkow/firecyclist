-- (c) Wilson Berkow

module Player where

import List
import Graphics.Collage (..)
import Color (..)
import Time

import Config (game_side_margin, game_top_margin, game_total_width, game_total_height)
import BasicUtil (..)
import HasPosition (..)
import Platfm (Platfm, configPlatfm, platfm_thickness)

type alias Player = { pos : Position, vel : Position }
type alias PlayerInputs = (List Platfm, Time.Time)

player_head_radius = 7
player_wheel_radius = player_head_radius * 7 / 6
player_torso_length = player_head_radius * 3 / 2

playerWheelCenterPos : Player -> Position
playerWheelCenterPos pl = vect_fall (player_head_radius + player_wheel_radius + player_torso_length) pl.pos

graphic =
  let rad = player_head_radius
      torsoLen = player_torso_length
      wheelRad = player_wheel_radius
      torsoBottom = -rad - torsoLen
      armsStartY = (-rad + torsoBottom) / 2
      elbowYDiff = rad / 4
      elbowXDiff = rad
      
      oneArm = group
        [ traced defaultLine (segment (0, 0)                    (-elbowXDiff,   elbowYDiff))
        , traced defaultLine (segment (-elbowXDiff, elbowYDiff) (-2*elbowXDiff, -elbowYDiff))
        ]
      
      oneSpoke = traced defaultLine (segment (0, -wheelRad) (0, wheelRad))
      allSpokes = group
        [ rotate (turns <| 0/6) oneSpoke
        , rotate (turns <| 1/6) oneSpoke
        , rotate (turns <| 2/6) oneSpoke
        , rotate (turns <| 3/6) oneSpoke
        , rotate (turns <| 4/6) oneSpoke
        , rotate (turns <| 5/6) oneSpoke
        --, rotate (turns <| 6/8) oneSpoke
        --, rotate (turns <| 7/8) oneSpoke
        ]
  in group
       [ -- Head:
         outlined (solid black) (circle rad)
         
         -- Torso:
       , traced defaultLine (segment (0, -rad) (0, -rad - torsoLen))
         
         -- Arms:
       , moveY armsStartY oneArm
       , moveY armsStartY (scale -1 oneArm)
         
         -- Wheel:
       , moveY (-rad - torsoLen - wheelRad) <| outlined (solid black) (circle wheelRad)
       , moveY (-rad - torsoLen - wheelRad) allSpokes
       ]

renderPlayer : Player -> Form
renderPlayer p = move_f p.pos graphic

intersects_plat : Player -> Platfm -> Bool
intersects_plat player plat = -- This algorithm is from http://mathworld.wolfram.com/Circle-LineIntersection.html
  let rad = player_wheel_radius + platfm_thickness
      ppos = playerWheelCenterPos player
      (startx, starty) = (min plat.start.x plat.end.x, min plat.start.y plat.end.y)
      (endx,   endy)   = (max plat.start.x plat.end.x, max plat.start.y plat.end.y)
  in in_range (startx - rad) (endx + rad) ppos.x && in_range (starty - rad) (endy + rad) ppos.y &&
       (let (px, py) = let {x,y} = ppos in (x,y)
            platlength = distance plat.start plat.end
            (offsetx0, offsety0) = let {x,y} = vect_subtract plat.start ppos in (x,y)
            (offsetx1, offsety1) = let {x,y} = vect_subtract plat.end ppos in (x,y)
            big_D = offsetx0 * offsety1 - offsetx1 * offsety0
        in (rad * platlength) ^ 2 >= big_D ^ 2)

touching_any : Player -> List Platfm -> Maybe Platfm
touching_any pl plats =
  case plats of
    p::ps ->
      if intersects_plat pl p
        then Just p
        else touching_any pl ps
    [] -> Nothing

stepPlayer : PlayerInputs -> Player -> Player
stepPlayer =
  let slope (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1)
      
      plat_slope : Platfm -> Float
      plat_slope {start, end} = slope (start.x, start.y) (end.x, end.y)
      platfm_bounciness = 0.75--1--.5--3 or 0, with lower sliding speed (vscale below)
      vel_from_slope dt m = { x = signnum m, y = abs m }
                          |> vscale 3--6
                          |> vect_rise configPlatfm.fall_rate
                          |> vect_rise platfm_bounciness
      player_grav = 0.3--0.2--0.3
      
      step (plats, dt) p =
        let on_plat = touching_any p plats
            vel =
              case on_plat of
                Just plat -> vel_from_slope dt (plat_slope plat)
                Nothing -> vect_fall (player_grav * dt / 28) p.vel
            (newx, newy) = let {x,y} = vect_add p.pos (vscale (dt / 20) vel) in (x,y)
            gwidth = toFloat game_total_width
            rad = player_wheel_radius -- This just represents how far the graphic of the player extends out from 0 in each direction in the X axis.
            modded_newx = if | newx > gwidth + rad -> newx - gwidth - rad
                             | newx < -rad         -> newx + gwidth + rad
                             | otherwise           -> newx
        in { pos = {x=modded_newx,y=newy}
           , vel = vel
           }
  in step