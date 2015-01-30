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

circleA r = filled red (circle r)
circleB r = filled darkGrey (circle r)

onlyFor : Float -> Float -> Form -> Form
onlyFor lifeleft frac graphic =
  if lifeleft >= frac * target_lifespan
    then graphic
    else graphic--toForm E.empty

renderTarget : Target -> Form
renderTarget (pos, life, _) =
  move_f pos <| scale (life / target_lifespan) <| group -- TODO: Take advantage of (HasPosition r), make each game object implement it (maybe other than Platfm?), and have Game.render move the objects to the right location. Conceptually, it doesn't make sense for that to be done here.
    [ onlyFor life 0.75 <| circleA target_radius
    , onlyFor life 0.5  <| circleB (target_radius * 0.75)
    , onlyFor life 0.25 <| circleA (target_radius * 0.5)
    , onlyFor life 0    <| circleB (target_radius * 0.25)
    ]