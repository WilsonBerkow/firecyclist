-- (c) Wilson Berkow.

import Signal (map, foldp, sampleOn)
import Time (inSeconds, fps)

import App

main = map App.render
         (foldp
           App.step
           App.init
           (sampleOn delta App.inputs))

delta = map inSeconds (fps 60)

-- IN THIS VERSION:
--  - Removed unnecessary and bad x > 100 restriction in resuming in Paused.
--  -
--  - TODO: Start using GlobalState, and test it out with features like "Mute" (even though
--     I still have no sound).
--  - TODO: Perhaps make a cmd-line tool compiling all CompPP things in folder? Idk
--     how to do that, but maybe all I'll have to do is a little Node.js....

-- *POTENTIAL* HUGE TODO_PROBLEM: Do I have a memory leak...? Cuz the graphics seems to get slower
--                                 after ~10 minutes of platfms falling, and that's... worrisome....
--                                 I should check on it.