-- (c) Wilson Berkow

module Fireball where

import Time (Time)
import List ((::))

import HasPosition (..)
import Graphics.Collage as Collage
import Color

type alias Fireball = { pos : Position, speed : Float }
type alias FireballInputs = Time

configFireball = let side_len = 12
                 in { def_speed = -2--3
                    , side_len = side_len
                    , padded_len = side_len * 1.5  -- This is for offsetting fireballs in sequence (i.e. padded_len is the distance from one's x-/y-pos to the next's x-/y-pos).
                    }
fb_radius = 10
fb_triangle_radius = 12
fb_height = fb_radius + 1.5 * fb_triangle_radius -- 1.5*circumradius = height for equilateral triangles
stepFireball : FireballInputs -> Fireball -> Fireball
stepFireball dt fb = { pos = vect_fall (dt / 20 * fb.speed) fb.pos, speed = fb.speed }

single_fb = Collage.group [
   Collage.move (0,-1.9*4) (Collage.rotate (turns 0.75) (Collage.filled Color.red (Collage.ngon 3 fb_triangle_radius)))
 , Collage.move (0,0) (Collage.filled Color.orange (Collage.circle fb_radius))
 , Collage.move (2.5-3,0) (Collage.outlined (Collage.solid Color.red) (Collage.circle fb_radius))
 ]

renderFireball f = move_f f.pos single_fb

makeFireball : Position -> Fireball
makeFireball pos = { pos = pos, speed = configFireball.def_speed }