-- (c) Wilson Berkow.
import Signal (map, foldp, sampleOn)
import Time (inSeconds, fps)
import App (cApp_render, cApp_step, cApp_init, cApp_inputs)
main = map cApp_render
         (foldp
           cApp_step
           cApp_init
           (sampleOn delta cApp_inputs))
delta = map inSeconds (fps 60)
-- IN THIS VERSION:
--  - Removed Markdown dependency, using Text functions instead (it was retarded
--     to have ever used Markdown for just boldening text).
--  - MODULES!!! Modularized everything I wanted: different parts of the logic,
--     basic util, and components are all in their own modules.
--  - TODO: Use CompPP for all appropriate, and extend CompPP for more precision,
--     which includes truly private local variables, adding of type signatures
--     in front of declarations of recognized props, and maybe 'within' flags.
--  - TODO: Start using GlobalState, and test it out with features like "Mute" (even though
--     I still have no sound).
--  - TODO: Perhaps make a cmd-line tool compiling all CompPP things in folder? Idk
--     how to do that, but maybe all I'll have to do is a little Node.js....
-- *POTENTIAL* HUGE TODO_PROBLEM: Do I have a memory leak...? Cuz the graphics seems to get slower
--                                 after ~10 minutes of platfms falling, and that's... worrisome....
--                                 I should check on it.