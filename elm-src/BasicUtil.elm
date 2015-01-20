-- (c) Wilson Berkow

module BasicUtil where

import List ((::), foldl)
import Color

x `mod` y = if | x < 0 -> y + x
               | x >= y -> x - y
               | otherwise -> x

isJust ma =
  case ma of
    Just a  -> True
    Nothing -> False

in_range x1 x2 v = (x1 <= v && v <= x2) || (x2 <= v && v <= x1)

mhead l = case l of (x::xs) -> Just x
                    []      -> Nothing
mtail l = case l of (x::xs) -> Just xs
                    []      -> Nothing
replicate : number -> a -> List a
replicate n x = if n == 0 then [] else x :: (replicate (n - 1) x)

deepGrey = Color.rgb 50 50 50

signnum x = if | x > 0 -> 1
               | x == 0 -> 0
               | x < 0 -> -1

fn_map f g = \x -> f (g x)

fn_map2 : (a -> b -> c) -> (x -> a) -> (x -> b) -> (x -> c)
fn_map2 f g h = \x -> f (g x) (h x)

any : (a -> Bool) -> List a -> Bool
any p = foldl (\elem acc -> acc || p elem) False