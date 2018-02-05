module Model.Chess exposing (..)

import Data.Type exposing (..)
import Model.FEN exposing (..)
import Material

init : Chess
init = let initBoard = fromFEN initialBoard
           initPlayer = Player Nothing Nothing
       in Chess initBoard initPlayer [] (UI Material.model "")
