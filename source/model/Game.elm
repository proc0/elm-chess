module Model.Game exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.Board exposing (..)
import Material

init : Game
init = let p1 = idlePlayer White
           p2 = idlePlayer Black
           ui = UI Material.model ""
           history = []
       in Game ui (p1, p2) initBoard history 
