-- (c) Wilson Berkow

module Fireball where

import Time (Time)
import List ((::))

import Vect (..)
import Graphics.Collage as Collage
import Color

type alias Fireball = { pos : Vect, speed : Float }
type alias FireballInputs = Time

configFireball = let side_len = 12
                 in { def_speed = -2--3
                    , side_len = side_len
                    , padded_len = side_len * 1.5  -- This is for offsetting fireballs in sequence (i.e. padded_len is the distance from one's x-/y-pos to the next's x-/y-pos).
                    }
tri : Float -> Collage.Shape -- Makes an equilateral triangle of side-length s, pointed upward (like /\, not \/).
tri s = let h = s * ((sqrt 3) / 2)
        in Collage.polygon [(-s/2, -h/2), (s/2, -h/2), (0, h/2)]
fb_height = 4 * 4.5
single_fb =
  let unit = 4 -- TODO!!!! Change the rest of the program to deal with these larger fireballs (I changed the unit from 3 to 4 because it looks better).
      sq = Collage.filled Color.darkOrange (Collage.square unit)
      at x y = Collage.move (x * unit, -y * unit) sq
  in Collage.moveY (4.5 * unit) <| Collage.group
      [ Collage.move (2.5*unit,-1.5*unit) (Collage.rotate (turns 0.75) (Collage.filled Color.orange (Collage.ngon 3 (3*unit))))
      
      , at 0   0
      , at 1   0
      , at 2   0
      , at 3   0
      , at 4   0
      , at 5   0
      
      , at 0.5 1
      , at 1   2
      , at 1.5 3
      , at 2   4
      
      , at 2.5 5
      , at 3   4
      , at 3.5 3
      , at 4   2
      , at 4.5 1
      ]

stepFireball : FireballInputs -> Fireball -> Fireball
stepFireball dt fb = { pos = vect_fall fb.speed fb.pos, speed = fb.speed }
renderFireball f = let base_clr = Color.orange--rgb 200 100 0
                   in single_fb
                        |> move_f f.pos

makeFireball : Vect -> Fireball
makeFireball pos = { pos = pos, speed = configFireball.def_speed }
staticFb x y = { pos = (Vect x y), speed = 0 }

fbCol amt (Vect x y) = if | amt > 0 -> (staticFb x y)
                                       :: fbCol (amt - 1) (Vect x (y + configFireball.padded_len))
                          | otherwise -> []
fbRow amt (Vect x y) = if | amt > 0 -> (staticFb x y)
                                       :: fbRow (amt - 1) (Vect (x + configFireball.padded_len) y)
                          | otherwise -> []