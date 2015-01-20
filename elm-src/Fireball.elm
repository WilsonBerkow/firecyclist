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
fb_height = 4 * 4.5 -- TODO: CHANGE THIS AND RELATED VARIABLES TO DESCRIBE THE NEWLY-SHAPED FIREBALLS.

stepFireball : FireballInputs -> Fireball -> Fireball
stepFireball dt fb = { pos = vect_fall (dt / 20 * fb.speed) fb.pos, speed = fb.speed }

single_fb = Collage.group [
   Collage.move (2.5*4,-1.9*4) (Collage.rotate (turns 0.75) (Collage.filled Color.red (Collage.ngon 3 (3*4))))
 , Collage.move (2.5*4,0) (Collage.filled Color.orange (Collage.circle 10))
 , Collage.move (2.5*5-3,0) (Collage.outlined (Collage.solid Color.red) (Collage.circle 10))
 ]

renderFireball f = single_fb |> move_f f.pos

makeFireball : Position -> Fireball
makeFireball pos = { pos = pos, speed = configFireball.def_speed }