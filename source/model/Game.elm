module Model.Game exposing (..)

import Data.Type exposing (..)
import Model.Board exposing (..)
import Material

init : Game
init = let white = Player White Nothing Nothing []
           black = Player Black Nothing Nothing []
           ui = UI Material.model ""
           history = []
       in Game ui (white,black) initBoard history 
