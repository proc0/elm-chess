module Toolkit exposing (..)

import Data.Game as G exposing (..)

(=>) = (,)

fst = Tuple.first
snd = Tuple.second

toPosition : (Int, Int) -> G.Position
toPosition (x_, y_) = 
          { x = x_
          , y = y_
          }