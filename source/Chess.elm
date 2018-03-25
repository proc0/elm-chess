module Chess exposing (main)

import Html       exposing (program)
import Tuple      exposing (mapFirst)
import Data.Type  exposing (Game, Event)
import Data.Pure  exposing (newGame, startCommands)
import Depo.FEN   exposing (fromFEN, openingBoard)
import State.Game exposing (update, subscribe)
import View.Main  exposing (render)
    
--  ♘ 

startGame : Game
startGame =
    let start = 
            newGame.chess
        newBoard = 
            fromFEN openingBoard
    in 
    ({ newGame
     | chess =
        { start
        | board =
            newBoard
        }
    })

main : Program Never Game Event
main = program
     { init          = startGame ! startCommands
     , view          = render
     , update        = update
     , subscriptions = subscribe
     }
    