module Model.Game exposing (..)

import Data.Type exposing (..)
import Model.FEN exposing (..)
import Material

init : Game
init = let board = fromFEN initialBoard
           white = Player White []
           black = Player Black []
           turn = Turn white Nothing Nothing
           ui = UI Material.model ""
       in Game ui (white,black) board turn [] 
