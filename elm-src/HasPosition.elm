-- (c) Wilson Berkow*

module HasPosition where

import List (foldl)
import Graphics.Collage (Form, move, Path, segment)

import Config (game_total_width, game_total_height)

type alias HasPosition r = { r | x : Float, y : Float }
type alias Position = { x : Float, y : Float }
vect_add xy xy' = { x = xy.x + xy'.x, y = xy.y + xy'.y }
vect_subtract xy xy' = { x = xy.x - xy'.x, y = xy.y - xy'.y }
vsum = foldl vect_add {x=0, y=0}
vect_fall dy r = { r | x <- r.x, y <- (r.y + dy) }
vect_rise dy r = { r | x <- r.x, y <- (r.y - dy) }
vscale s r = { r | x <- (s * r.x), y <- (s * r.y) }

distance p0 p1 = sqrt <| (p1.x - p0.x) ^ 2 + (p1.y - p0.y) ^ 2

set_origin_context : (Int,Int) -> HasPosition r -> HasPosition r
set_origin_context (total_width, total_height) r = { r | x <- r.x - (toFloat total_width / 2), y <- -r.y + (toFloat total_height / 2) }

fix_origin = set_origin_context (game_total_width, game_total_height)

pt_to_fp : HasPosition r -> (Float, Float) -- pt_to_fp for "point to Float-pair"
pt_to_fp pt = let {x, y} = fix_origin pt
              in (x, y)
move_f : HasPosition r -> Form -> Form
move_f pt form = move (pt_to_fp pt) form

segment_f : HasPosition r -> HasPosition r -> Path
segment_f pt1 pt2 = segment (pt_to_fp pt1) (pt_to_fp pt2)