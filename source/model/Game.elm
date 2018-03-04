module Model.Game exposing (..)

import Mouse exposing (..)
import Material.Layout as Layout

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

subscriptions : Game -> Sub Event
subscriptions { ui, players } = 
    let player = fst players
        layout = Layout.subs Mdl ui.mdl
    in -- if player 
    case player.action of
        -- is moving
        Moving _ -> 
            Sub.batch 
                -- track position
                [ Mouse.moves Drag
                , Mouse.ups Drop
                , layout
                ]
        _ -> layout