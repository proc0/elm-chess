module Data.Pure exposing (..)

import Matrix exposing (Matrix, Location, loc, fromList)
import Mouse exposing (Position)
import Material
import Material.Layout as Layout

import Data.Type exposing (..)

-- Nuetral type instances

newGame : Game
newGame = 
    let chess =
            Chess nullBoard newHistory
        players = 
            ( newPlayer White
            , newPlayer Black
            )
        ui = 
        UI Material.model "" False
    in 
    Game ui chess players

startCommands : List (Cmd Event)
startCommands = 
    [Layout.sub0 GUI]

origin : Location
origin = loc 0 0

originPos : Position
originPos = { x=0, y=0 }

joker : Piece
joker = 
    Piece origin originPos Black Joker 0 False False []

vacantSquare : Square
vacantSquare = 
    Square origin Nothing False False

nullBoard : Board
nullBoard =
    fromList [[vacantSquare]]

newPlayer : Color -> Player 
newPlayer color =
    Player color (toString color ++ " Player") Idle []

noMove : Move 
noMove =
    Move origin origin joker Nothing Nothing False False

newHistory : History
newHistory = []
