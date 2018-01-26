module Frame.Main exposing (..)

import Array exposing (..)
import Matrix exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Mouse exposing (..)
import Json.Decode as Json exposing (..)
import Debug exposing (..)

import Data.Main exposing (..)
import Data.Game as Game exposing (..)
import Frame.Moves exposing (..)
import Settings exposing (..)
import Toolkit exposing (..)

onMouseDown : Attribute Msg
onMouseDown = on "mousedown" (Json.map Click Mouse.position)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ game, select, player } as model) =
    let 
        --logMovement = case msg of
        --    Click ps -> log "Click" ps
        --    Drag pc ps -> log "Drag" ps
        --    Drop pc ps -> log "Drop" ps

        nextMovement : Maybe Moving
        nextMovement = case msg of
            Click ps -> startDrag ps player game.board
            Drag pc ps -> Maybe.map (updateMoving ps pc) player
            Drop pc ps -> Nothing

        nextSelect : Maybe Square
        nextSelect = case msg of
            Click ps -> Just (findSquare (toGamePosition ps) game.board)
            Drag pc ps -> select            
            Drop pc ps -> 
                case select of
                    Just {position, piece} ->
                        case piece of
                            Just p -> 
                                -- prevents select from persisting after dragging piece
                                if p == pc && position /= (toGamePosition ps)
                                then Nothing
                                else Just (Square position piece True)
                            Nothing -> Nothing
                    Nothing -> Nothing            

        nextGame : Chess
        nextGame = case msg of
            Click ps -> 
                case nextMovement of
                     Just mv -> 
                        case mv.piece of
                            Just pc -> 
                                let movingSquare = findSquare (toGamePosition mv.start) game.board
                                in Chess (validate movingSquare <| liftPiece pc mv.start (clearBoardHilite game.board)) game.history
                            Nothing -> game
                     -- case for moving by clicking squares
                     Nothing -> 
                        case select of
                            Just ({position,piece} as sel) -> 
                                let validBoard = validate sel game.board
                                    nextSquare = findSquare (toGamePosition ps) validBoard
                                in case piece of
                                        Just p -> 
                                            if nextSquare.valid
                                            then Chess (clearBoardHilite <| (liftPiece2 p position) <| (addPiece p ps) <| validBoard) game.history
                                            else Chess (clearBoardHilite <| (addPiece2 p position) <| (allValid game.board)) game.history
                                        Nothing -> game
                            Nothing -> game
            Drop pc ps -> 
                case select of
                    Just ({position,piece} as sel) -> 
                        let validBoard = validate sel game.board
                            nextSquare = findSquare (toGamePosition ps) validBoard
                        in case piece of
                                Just p -> 
                                    if nextSquare.valid 
                                    then Chess (clearBoardHilite <| addPiece pc ps validBoard) game.history
                                    else Chess (clearBoardHilite <| addPiece2 p position (allValid game.board)) game.history
                                Nothing -> game
                    Nothing -> game    
            Drag _ _ -> game
            
    in (Model nextGame nextSelect nextMovement, Cmd.none)

startDrag : Mouse.Position -> Maybe Moving -> Board -> Maybe Moving
startDrag ps pl board = 
    let sq = findSquare (toGamePosition ps) board
    in case sq.piece of
            Just p -> Just (Moving (Just p) ps ps)
            Nothing -> Nothing

updateDrag : Mouse.Position -> Moving -> Moving
updateDrag ps {start} = Moving Nothing start ps

updateMoving : Mouse.Position -> Piece -> Moving -> Moving
updateMoving ps pc {start} = Moving (Just pc) start ps 

stopMoving : Mouse.Position -> Piece -> Moving -> Maybe Moving
stopMoving ps pc {start} = Just (Moving Nothing start ps)

--findPiece : Mouse.Position -> Board -> Maybe Piece
--findPiece ps board = 
--    let sq = (flip findSquare board) <| toGamePosition ps
--    in sq.piece

findSquare : Game.Position -> Game.Board -> Square
findSquare pos board = 
    let emptySquare = Square pos Nothing False
        retrieve : Int -> List a -> Maybe a
        retrieve index items = 
                items 
                |> Array.fromList 
                |> Array.get index
        getSquare b = 
            retrieve pos.y b 
            |> Maybe.andThen (retrieve pos.x)

    in case getSquare board of
            Just match -> match
            Nothing -> emptySquare

-- board manipulations
----------------------

clearBoardHilite : Board -> Board
clearBoardHilite board = 
        List.map (\rk -> List.map (\{position,piece,valid} -> Square position piece False) rk) board

allValid : Board -> Board
allValid board = 
        List.map (\rk -> List.map (\{position,piece,valid} -> Square position piece True) rk) board

liftPiece : Piece -> Mouse.Position -> Board -> Board
liftPiece pc ps bd = updateBoard remPieceFromSquare pc ps bd

liftPiece2 : Piece -> Game.Position -> Board -> Board
liftPiece2 pc ps bd = updateBoard remPieceFromSquare2 pc ps bd

addPiece : Piece -> Mouse.Position -> Board -> Board
addPiece pc ps bd = updateBoard addPieceToSquare pc ps bd

addPiece2 : Piece -> Game.Position -> Board -> Board
addPiece2 pc ps bd = updateBoard addPieceToSquare2 pc ps bd

updateBoard : (Piece -> Game.Position -> Square -> Square) -> Piece -> Game.Position -> Board -> Board
updateBoard trans piece pos board =
    List.map ((\pc ps rk -> List.map (trans pc ps) rk) piece pos) board

remPieceFromSquare : Piece -> Game.Position -> Square -> Square
remPieceFromSquare pc pos sq = 
        case sq.piece of 
            Just pec -> 
                if sq.position == (toGamePosition pos) && pec == pc 
                then Square sq.position Nothing True
                else sq
            Nothing -> sq

remPieceFromSquare2 : Piece -> Game.Position -> Square -> Square
remPieceFromSquare2 pc pos sq = 
        case sq.piece of 
            Just pec -> 
                if sq.position == pos && pec == pc 
                then Square sq.position Nothing True
                else sq
            Nothing -> sq

addPieceToSquare : Piece -> Mouse.Position -> Square -> Square
addPieceToSquare pc mp sq =
        case sq.piece of 
            Just pec ->
                if sq.position == (toGamePosition mp) && sq.valid
                then Square sq.position (Just pc) True 
                else sq
            Nothing -> 
                if sq.position == (toGamePosition mp) && sq.valid
                then Square sq.position (Just pc) True 
                else sq 

addPieceToSquare2 : Piece -> Game.Position -> Square -> Square
addPieceToSquare2 pc mp sq =
        case sq.piece of 
            Just pec ->
                if sq.position == mp && sq.valid
                then Square sq.position (Just pc) True 
                else sq
            Nothing -> 
                if sq.position == mp && sq.valid
                then Square sq.position (Just pc) True 
                else sq 

-----

validate : Square -> Board -> Board
validate sq bd =
    let possibleSquares = getPossibleSquares sq bd
        -- using foldl for hack to check memo square
        -- todo: process possible squares as matrix
        checkMoves sq_ = List.foldl 
            (\s {position,piece,valid} -> 
                if s.position == position
                then Square position piece True
                else Square position piece valid 
            ) sq_ possibleSquares
    in List.map (\rk -> List.map checkMoves rk) (Matrix.toList bd) |> Matrix.fromList

getPossibleSquares : Square -> Board -> List Square
getPossibleSquares sq bd = (flip filterSameSquares) bd <| getPossible sq bd

filterSameSquares : List Square -> Board -> List Square
filterSameSquares squares bd =
    let checkSquare sq_ s =
            sq_.position == s.position
        checkRank s_ rk = List.head <| List.filter (checkSquare s_) rk
        filterSquare square =
            let s1 = List.filterMap (checkRank square) (Matrix.toList bd)
            in case (List.head s1) of
                 Just s -> filterSame square s
                 Nothing -> False
    in List.filter filterSquare squares

filterSame : Square -> Square -> Bool
filterSame s1 s2 = 
    let isWhite p =
            case p of
                White _ -> True
                Black _ -> False
        avoidWhite p =
            case p of
                White _ -> False
                Black _ -> True
        avoidBlack p =
            case p of
                White _ -> True
                Black _ -> False
        checkRule p1 p2 =
            if isWhite p1
            then avoidWhite p2
            else avoidBlack p2
        valid = Maybe.map2 checkRule s1.piece s2.piece 
    in case valid of
        Just v -> v
        Nothing -> True

