module Data.Pure exposing (..)

import Matrix exposing (Matrix, Location, loc)
import Mouse exposing (Position)
import Material
import Material.Layout as Layout

import Data.Type exposing (..)
import Model.FEN exposing (..)

-- Nuetral type instances

newGame : (Game, Cmd Event)
newGame = 
    let chess =
            Chess (fromFEN initialBoard) [] 
        players = 
            ( newPlayer White
            , newPlayer Black
            )
        -- UI events and subs
        ui = UI Material.model "" False
        cmd = [Layout.sub0 Mdl]
    in 
    Game ui chess players ! cmd

zeroLoc : Location
zeroLoc = loc 0 0

zeroPs : Position
zeroPs = { x=0, y=0 }

nullPiece : Piece
nullPiece = 
    Piece zeroPs zeroLoc Black Joker 0 []

vacantSquare : Square
vacantSquare = 
    Square zeroLoc Nothing False False

newPlayer : Color -> Player 
newPlayer color =
    Player color (toString color ++ " Player") Idle []

noMove : Move 
noMove =
    Move zeroLoc zeroLoc nullPiece Nothing False
