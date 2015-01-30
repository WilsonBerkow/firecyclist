-- (c) Wilson Berkow

module Target (Target, TargetInputs, stepTarget, renderTarget, target_radius, target_lifespan) where

import Graphics.Element as E
import Graphics.Collage (..)
import Color (..)
import Time

import HasPosition (..)

type alias Target = (Position, Time.Time, Maybe Time.Time) -- I'm beginning to think my frequent use of type aliases is bad practice, and that I should instead use data (or newtype style).... TODO: Ponder this
-- The (Maybe Time.Time) represents the time that the obtaining-animation has been
--  running, and whether or not it has happened yet (determined by the player
--  hitting the target). This I have not implemented yet, and am leaving there
--  for the future.

type alias TargetInputs = Time.Time

target_radius = 12
target_lifespan = 6000

stepTarget : TargetInputs -> Target -> Target
stepTarget dt (pos, life, a) = (pos, life - dt, a)

circA = filled red (circle target_radius)
circB = filled darkGrey (circle target_radius)

graphic = group -- TODO: Take advantage of (HasPosition r), make each game object implement it (maybe other than Platfm?), and have Game.render move the objects to the right location. Conceptually, it doesn't make sense for that to be done here.
  [            circA
  , scale 0.75 circB
  , scale 0.5  circA
  , scale 0.25 circB
  ]

renderTarget : Target -> Form
renderTarget (pos, life, _) =
  move_f pos <| scale (life / target_lifespan) <| graphic