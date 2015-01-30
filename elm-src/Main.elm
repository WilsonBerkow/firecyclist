-- (c) Wilson Berkow.

import Signal (Signal, map, map2, map3, foldp, sampleOn)
import Time (Time, inSeconds, fps)
import Config (framerate)
import Maybe (Maybe, withDefault)
import Touch
import BasicUtil (..)
import HasPosition (Position)

import App

main = map App.render
         (foldp
           App.step
           App.init
           (sampleOn delta (map2 (,) approx_time inputs)))

delta = fps framerate

inputs = map3 (,,) cur_touch (map (withDefault {x=0,y=0}) taps) delta

port touches : Signal (List Touch.Touch)
port touches = Touch.touches

port cur_touch : Signal (Maybe Touch.Touch)

port taps : Signal (Maybe Position)

port approx_time : Signal Time

-- TODO: Start using GlobalState, and test it out with highscores storage. That
--  would most likely not be done with ordinary global state, as it would have
--  to persist across sessions, but it is a good start to get into global state,
--  which will be necessary for things like Mute.