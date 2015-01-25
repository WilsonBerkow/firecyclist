-- (c) Wilson Berkow

module Coin (Coin, CoinInputs, stepCoin, renderCoin, coin_radius) where

import Graphics.Element as Element
import Graphics.Collage (..)
import Color (..)
import Time

import HasPosition (..)

type alias Coin = Position
type alias CoinInputs = Time.Time

coin_radius = 10
coin_fall_rate = 2

stepCoin : CoinInputs -> Coin -> Coin
stepCoin dt st =
  { x = st.x, y = st.y - coin_fall_rate * dt / 20}

renderCoin : Coin -> Form
renderCoin st =
  let rad = coin_radius
      w = 8.5
  in move_f st (group
       [ filled yellow (circle rad)
       , outlined (solid darkOrange) (circle rad)
       , filled darkOrange (rect w w)
       , outlined (solid orange) (rect w w)
       ])