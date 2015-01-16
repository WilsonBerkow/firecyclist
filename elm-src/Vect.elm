-- (c) Wilson Berkow

module Vect where

import List (foldl)
import Graphics.Collage (Form, move, Path, segment)

import Config (game_total_width, game_total_height)

type Vect = Vect Float Float
vect_add (Vect x y) (Vect x' y') = Vect (x + x') (y + y')
vsum = foldl vect_add (Vect 0 0)
vect_fall dy (Vect x y) = Vect x (y + dy)
vect_rise dy (Vect x y) = Vect x (y - dy)
vscale s (Vect x y) = Vect (s * x) (s * y)

vect_x (Vect x _) = x
vect_y (Vect _ y) = y

distance (Vect x0 y0) (Vect x1 y1) = sqrt ((x1 - x0)^2 + (y1 - y0)^2)

set_origin_context : (Int,Int) -> Vect -> Vect
set_origin_context (total_width, total_height) (Vect x y) = Vect (x - (toFloat total_width / 2)) (-y + (toFloat total_height / 2))

fix_origin = set_origin_context (game_total_width, game_total_height)

pt_to_fp : Vect -> (Float, Float) -- pt_to_fp for "point/Vect to Float-pair"
pt_to_fp pt = let (Vect x y) = fix_origin pt
              in (x, y)
move_f : Vect -> Form -> Form
move_f pt form = move (pt_to_fp pt) form

segment_f : Vect -> Vect -> Path
segment_f pt1 pt2 = segment (pt_to_fp pt1) (pt_to_fp pt2)