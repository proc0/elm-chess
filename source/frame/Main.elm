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
            Click ps   -> startDrag ps player game.board
            Drag pc ps -> Maybe.map (\{start} -> Moving (Just pc) start ps) player
            Drop pc ps -> Nothing

        nextSelect : Maybe Square
        nextSelect = case msg of
            Click ps   -> Just (findSquare (toGamePosition ps) game.board)
            Drag pc ps -> select            
            Drop pc ps -> select |> 
                -- prevents last piece from persisting
                Maybe.map (\{position, piece} ->
                    case piece of
                        Just p -> 
                            let movingAway = 
                                p == pc && position /= (toGamePosition ps)
                            in 
                            if movingAway
                            then Nothing -- clear select
                            else Just (Square position piece True)
                        Nothing -> Nothing) 
                |> Maybe.withDefault Nothing

        nextGame : Chess
        nextGame = case msg of
            Click ps -> 
                case nextMovement of
                     Just mv -> 
                        case mv.piece of
                            Just pc -> 
                                let movingSquare = findSquare (toGamePosition mv.start) game.board
                                in Chess (validate movingSquare <| liftPiece pc mv.start (toggleValid False game.board)) game.history
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
                                            then Chess (toggleValid False <| (liftPiece2 p position) <| (addPiece p ps) <| validBoard) game.history
                                            else Chess (toggleValid False <| (addPiece2 p position) <| (toggleValid True game.board)) game.history
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
                                    then Chess (toggleValid False <| addPiece pc ps validBoard) game.history
                                    else Chess (toggleValid False <| addPiece2 p position (toggleValid True game.board)) game.history
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

findSquare : Game.Position -> Game.Board -> Square
findSquare pos board = 
    let sq = Matrix.get (fromPos pos) board
        emptySquare = Square pos Nothing False
    in case sq of
            Just s -> s
            Nothing -> emptySquare

-- board manipulations
----------------------

toggleValid : Bool -> Board -> Board
toggleValid isValid board=
        Matrix.map (\{position,piece} -> Square position piece isValid) board

--clearBoardHilite : Board -> Board
--clearBoardHilite board = 
--        Matrix.map (\{position,piece,valid} -> Square position piece False) board
--        --List.map (\rk -> List.map (\{position,piece,valid} -> Square position piece False) rk) board

--allValid : Board -> Board
--allValid board = 
--        Matrix.map (\{position,piece,valid} -> Square position piece True) board
        --List.map (\rk -> List.map (\{position,piece,valid} -> Square position piece True) rk) board

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
    Matrix.map (\sq -> trans piece pos sq) board
    --List.map ((\pc ps rk -> List.map (trans pc ps) rk) piece pos) board

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
    let validSquares = getValidSquares sq bd
        checkMoves sq_ b = 
            Matrix.update (fromPos sq_.position) 
                (\{ position, piece, valid } ->
                    Square position piece True) b 
    in List.foldl checkMoves bd validSquares

getValidSquares : Square -> Board -> List Square
getValidSquares sq bd = (flip filterSameSquares) bd <| getPossible sq bd

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

