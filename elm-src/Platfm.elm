-- (c) Wilson Berkow

module Platfm where

import Time (Time, inSeconds)
import Graphics.Collage as Collage
import Color

import BasicUtil (..)
import HasPosition (..)

type alias Platfm =
  { start : Position
  , end : Position
  , time_left : Time
  }
type alias PlatfmInputs = Time
configPlatfm = { fall_rate = 3 } -- TODO: Remove this variable, lift fall_rate to global ns or another config module.

stepPlatfm : Time -> Platfm -> Platfm
stepPlatfm dt {start, end, time_left} =
  let fall_rate = configPlatfm.fall_rate * dt / 20
  in { start = (vect_rise fall_rate start), end = (vect_rise fall_rate end), time_left = time_left - dt }
platfmLineStyle =
 let df = Collage.defaultLine
 in { df | width <- 6
         , cap <- Collage.Round
         , join <- Collage.Smooth
         }
renderPlatfm p = Collage.traced
                   { platfmLineStyle | color <- Color.rgba 0 0 0 (inSeconds p.time_left) }
                   (segment_f p.start p.end)

platfmPreviewLineStyle =
  { platfmLineStyle | color <- Color.rgba 150 150 150 0.5 }
renderTouchPlatfmPreview p = Collage.traced platfmPreviewLineStyle <| segment_f p.start p.end