module Frame.Main exposing (..)

import Array exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Mouse exposing (..)
import Json.Decode as Json exposing (..)
import Debug exposing (..)

import Data.Main exposing (..)
import Data.Game as Game exposing (..)
import Settings exposing (..)
import Toolkit exposing (..)

onMouseDown : Attribute Msg
onMouseDown =
  on "mousedown" (Json.map Grab Mouse.position)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ game, select, player } as model) =
    let nextMovement : Maybe Moving
        nextMovement = case msg of
            Grab ps -> Just (startDrag ps game.board)
            Drag ps -> Maybe.map (updateDrag ps) player
            Drop _  -> Nothing
            PieceDrag pc ps -> Maybe.map (updateMoving ps pc) player
            PieceDrop _ _ -> Nothing

        nextGame : GameModel
        nextGame = case msg of
            Grab ps -> 
                case nextMovement of
                     Just mv -> case mv.piece of
                                    Just pc -> GameModel (removePiece pc mv.start game.board) game.history
                                    _ -> game
                     _ -> game
            PieceDrop pc ps -> GameModel (addPiece pc ps game.board) game.history       
            _ -> game 
    in (Model nextGame select nextMovement, Cmd.none)
        
        --pieceLifted : Maybe Piece
        --pieceLifted = case msg of
        --    Grab ps -> 
        --         case nextMovement of
        --              Just mv -> mv.piece
        --              Nothing -> Nothing
        --    _ -> Nothing

        --nextGame : GameModel
        --nextGame = 
        --    case pieceLifted of
        --         Just p -> 
        --            GameModel (removePiece p game.board) game.select game.history
        --         Nothing -> game


startDrag : Mouse.Position -> Board -> Moving
startDrag ps board = Moving ps ps (findPiece ps board)
        
updateDrag : Mouse.Position -> Moving -> Moving
updateDrag ps {start} = Moving start ps Nothing

updateMoving : Mouse.Position -> Piece -> Moving -> Moving
updateMoving ps pc {start} = Moving start ps (Just pc)

findPiece : Mouse.Position -> Board -> Maybe Piece
findPiece ps board = 
    let sq = (flip findSquare board) <| toGamePosition ps
    in case sq of
            Occupied _ pc -> Just pc
            Vacant _ -> Nothing

findSquare : Game.Position -> Game.Board -> Square
findSquare pos board = 
    let retrieve index items = 
            case items of 
                 Just list -> list 
                    |> Array.fromList 
                    |> Array.get index
                 Nothing -> Nothing
        getSelection b = 
            retrieve pos.x <| 
            retrieve pos.y <| Just b
    in case getSelection board of
            Just match -> match
            Nothing -> Vacant pos

------


removePiece : Piece -> Game.Position -> Board -> Board
removePiece pc ps bd = updateBoard updateSquare pc ps bd


updateBoard : (Piece -> Game.Position -> Square -> Square) -> Piece -> Game.Position -> Board -> Board
updateBoard trans piece pos board =
    List.map ((\pc ps rk -> List.map (trans pc ps) rk) piece pos) board

--removePiece piece pos board = 
--    List.map (updateRank piece pos) board

--updateRank : Piece -> Game.Position -> Rank -> Rank
--updateRank pc ps rk = List.map (updateSquare pc ps) rk

updateSquare : Piece -> Game.Position -> Square -> Square
updateSquare pc pos sq = 
        case sq of 
            Occupied ps pec -> 
                if ps == (toGamePosition pos) && pec == pc 
                then Vacant ps 
                else Occupied ps pec
            Vacant ps -> Vacant ps

--------

addPiece : Piece -> Game.Position -> Board -> Board
addPiece pc ps bd = updateBoard upSquare pc ps bd
--addPiece piece pos board = 
--    List.map (upRank piece pos) board

--upRank : Piece -> Game.Position -> Rank -> Rank
--upRank pc ps rk = List.map (upSquare pc ps) rk

upSquare : Piece -> Game.Position -> Square -> Square
upSquare pc mp sq =
        case sq of 
            Occupied ps pec ->
                if ps == (toGamePosition mp)
                then Occupied ps pc 
                else Occupied ps pec
            Vacant ps -> 
                if ps == (toGamePosition mp)
                then Occupied ps pc 
                else Vacant ps 
--PieceMove move ->
--        { game =
--           { board = updateBoard move game.board
--           , select = let m2 = snd move 
--                      in case m2 of
--                              Occupied pos pc -> findSquare pos game.board
--                              Vacant pos      -> findSquare pos game.board
--           , history = game.history
--           }
--        , position = getPosition model
--        , drag = Nothing     
--        }

--onSquareClicked : Game.Position -> Model -> ( Model, Cmd Msg )
--onSquareClicked position model =
--    let prevSelection = model.game.select
--        nextModel = processMouseUp position model
--    in case prevSelection of
--            Occupied _ _ -> update (PieceMove (prevSelection, nextModel.game.select)) model
--            Vacant _ -> ( nextModel, Cmd.none )

--processMouseUp : Game.Position -> Model -> Model
--processMouseUp position ({ game } as model) = 
--    { game = 
--        { board = game.board
--        , select = findSquare position game.board
--        , history = game.history
--        }
--    , position = getPosition model
--    , drag = Nothing  
--    }


--getPosition : Model -> Mouse.Position
--getPosition {game, position, drag} =
--  case drag of
--    Nothing -> position

--    Just {start,current} ->
--        let curPos = toPosition (current.x, current.y)
--            gamPos = toPosition ((position.x + curPos.x - start.x), (position.y + curPos.y - start.y))
--        in Mouse.Position gamPos.x gamPos.y

--updateBoard : Move -> Board -> Board
--updateBoard (m1, m2) board = 
--      let movingPc = case m1 of 
--                          Occupied ps pc -> Just pc
--                          Vacant ps -> Nothing
--      in case m2 of
--              Occupied ps pc -> case movingPc of
--                                     Just p -> List.map (updateRank p ps) board
--                                     Nothing -> board
--              Vacant ps -> board

--updateRank : Piece -> Game.Position -> Rank -> Rank
--updateRank pc ps rk = List.map (updateSquare pc ps) rk

--updateSquare : Piece -> Game.Position -> Square -> Square
--updateSquare pc pos sq = 
--      let isSq p = p.x == pos.x && p.y == pos.y
--      in case sq of 
--          Occupied ps pec -> if isSq ps then Occupied ps pc else Occupied ps pec
--          Vacant ps -> if isSq ps then Occupied ps pc else Vacant ps
