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
update msg ({ game, player } as model) =
    let 
        nextMovement : Maybe Moving
        nextMovement = case msg of
            Click xy   -> startDrag xy game.board
            Drag sq xy -> updateDrag xy sq player
            Drop _ _   -> Maybe.map (\pl -> { pl | drag = Nothing }) player

            --Drop sq xy -> player.drag |> 
            --    -- prevents sticky piece when lifted
            --    Maybe.map (\{position, piece} ->
            --        case piece of
            --            Just p -> 
            --                let movingAway = 
            --                    p == piece && position /= (getPosition xy)
            --                in 
            --                if movingAway
            --                then Nothing -- drop drag
            --                else Just (Square position piece True)
            --            Nothing -> Nothing) 
            --    |> Maybe.withDefault Nothing

        --nextSelect : Maybe Square
        --nextSelect = case msg of
        --    Click xy   -> Matrix.get (toLocation xy) game.board
        --    Drag _ _   -> player.drag            
        --    Drop sq xy -> player.drag |> 
        --        -- prevents sticky piece when lifted
        --        Maybe.map (\{position, piece} ->
        --            case piece of
        --                Just p -> 
        --                    let movingAway = 
        --                        p == pc && position /= (getPosition ps)
        --                    in 
        --                    if movingAway
        --                    then Nothing -- drop drag
        --                    else Just (Square position piece True)
        --                Nothing -> Nothing) 
        --        |> Maybe.withDefault Nothing
        logSquare = case nextMovement of
            Just m -> case msg of
                Click ps -> log "Click" Nothing
                Drag pc ps -> log "Drag" (Just pc)
                Drop pc ps -> log "Drop" (Just pc)
            Nothing -> log "nothing" Nothing
        logMovement = case nextMovement of
            Just m -> case msg of
                Click ps -> log "Click" ps
                Drag pc ps -> log "Drag" ps
                Drop pc ps -> log "Drop" ps
            Nothing -> log "nothing" (toPosition (0,0))

        nextGame : Maybe Chess
        nextGame = case msg of
            Click xy -> 
                nextMovement |> Maybe.map (\mv ->
                    --Maybe.map (\sq -> 
                    --    Maybe.map (\pc -> 
                    --            Chess (validate sq <| liftPiece sq (toggleValid False game.board)) game.history
                    --        ) sq.piece
                    --    ) mv.drag)
                    --|> Maybe.map (\)
                    case mv.drag of
                        Just sq -> 
                            case sq.piece of
                                Just pc -> 
                                    case mv.select of
                                        Just sel -> Chess (validate sel <| liftPiece sel (toggleValid False game.board)) game.history
                                        Nothing -> game
                                    --let movingSquare = findSquare (getPosition mv.start) game.board
                                    --in Chess (validate movingSquare <| liftPiece pc mv.start (toggleValid False game.board)) game.history
                                Nothing -> game
                        -- case for moving by clicking squares
                        Nothing -> 
                            case mv.select of
                                Just ({position,piece} as sel) -> 
                                    let validBoard = validate sel game.board
                                        nextSquare = findSquare (getPosition xy) validBoard
                                    in case piece of
                                            Just p -> 
                                                if nextSquare.valid
                                                then Chess (toggleValid False <| (liftPiece sel) <| (addPiece ({ sel | position = getPosition xy })) <| validBoard) game.history
                                                else Chess (toggleValid False <| (addPiece ({ sel | position = getPosition xy })) <| (toggleValid True game.board)) game.history
                                            Nothing -> game
                                Nothing -> game
                    )
            Drop sq xy -> 
                nextMovement |> Maybe.map (\mv -> 
                    case mv.select of
                        Just ({position,piece} as sel) -> 
                            let validBoard = validate sel game.board
                                --nextSquare = findSquare (getPosition xy) validBoard
                            in case piece of
                                    Just _ -> 
                                        if sq.valid 
                                        then Chess (toggleValid False <| addPiece ({ sel | position = getPosition xy }) validBoard) game.history
                                        else Chess (toggleValid False <| addPiece ({ sel | position = getPosition xy }) (toggleValid True game.board)) game.history
                                    Nothing -> game
                        Nothing -> game
                    ) 
            Drag _ _ -> Nothing
            
    in (Model (Maybe.withDefault game nextGame) nextMovement, Cmd.none)

startDrag : Mouse.Position -> Board -> Maybe Moving
startDrag ({x,y} as xy) board = 
    let currentSquare = Matrix.get (toLocation << getPosition <| xy) board
        _ = log "currentSquare" currentSquare
    in Just <| Moving currentSquare (Maybe.map (\sq -> { sq | position = xy }) currentSquare)

updateDrag : Mouse.Position -> Square -> Maybe Moving -> Maybe Moving
updateDrag xy sq player = 
    --let pos = {x=xy.x,y=xy.y}
    Maybe.map (\p -> { p | 
            drag = Just ({ sq | position = xy })
        }) player
    --let currentSquare = Matrix.get (toLocation xy) board
    --in Maybe.map (\sq -> Moving sq sq) currentSquare

    --let sq = findSquare (getPosition xy) board
    --in case sq.piece of
    --        Just p -> Just (Moving (Just p) xy xy)
    --        Nothing -> Nothing

findSquare : Game.Position -> Game.Board -> Square
findSquare pos board = 
    let sq = Matrix.get (toLocation pos) board
        emptySquare = Square pos Nothing False
    in case sq of
            Just s -> s
            Nothing -> emptySquare

-- board manipulations
----------------------

toggleValid : Bool -> Board -> Board
toggleValid isValid board=
        Matrix.map (\{position,piece} -> Square position piece isValid) board

liftPiece : Square -> Board -> Board
liftPiece sq bd = 
    Matrix.update (toLocation sq.position) (\s -> { s | piece = Nothing }) bd

--updateBoard : (Square -> Square -> Square) -> Square -> Board -> Board
--updateBoard trans piece pos board =
--    Matrix.map (\sq -> trans piece pos sq) board

--liftPiece : Square -> Board -> Board
--liftPiece sq bd = updateBoard clearSquare sq bd

addPiece : Square -> Board -> Board
addPiece sq bd = 
    Matrix.update (toLocation sq.position) (\s -> { s | piece = sq.piece }) bd



--liftPiece : Piece -> Mouse.Position -> Board -> Board
--liftPiece pc xy bd = updateBoard remPieceFromSquare pc ps bd

--addPiece : Piece -> Game.Position -> Board -> Board
--addPiece pc ps bd = updateBoard addPieceToSquare pc ps bd

--updateBoard : (Piece -> Game.Position -> Square -> Square) -> Piece -> Game.Position -> Board -> Board
--updateBoard trans piece pos board =
--    Matrix.map (\sq -> trans piece pos sq) board

--clearSquare : Square -> Square -> Square
--clearSquare current target = 
--        case target.piece of 
--            Just pc -> 
--                if target.position == current.position && pc == current.piece 
--                then Square target.position Nothing True
--                else target
--            Nothing -> target

moveSquare : Square -> Square -> Square
moveSquare current target = 
        if target.position == current.position && target.valid 
        then { current | valid = True }
        else target

        --case sq.piece of 
        --    Just pec ->
        --        if sq.position == mp && sq.valid
        --        then Square sq.position (Just pc) True 
        --        else sq
        --    Nothing -> 
        --        if sq.position == mp && sq.valid
        --        then Square sq.position (Just pc) True 
        --        else sq 

-----

validate : Square -> Board -> Board
validate sq bd =
    let validSquares = getValidSquares sq bd
    --let validSquares = [sq]
        _ = log "valid" validSquares
        checkMoves sq_ b = 
            Matrix.update (toLocation sq_.position) 
                (\{ position, piece, valid } ->
                    Square position piece True) b 
    in List.foldl checkMoves bd validSquares

getValidSquares : Square -> Board -> List Square
getValidSquares sq bd = (flip filterSameSquares) bd <| getPossible sq bd

filterSameSquares : List Square -> Board -> List Square
filterSameSquares squares bd =
    let filterSquare target =
            let square = Matrix.get (toLocation target.position) bd
            in case square of
                Just sq -> isSameColor sq target
                Nothing -> False
            --let s1 = List.filterMap (checkRank square) (Matrix.toList bd)
            --in case (List.head s1) of
            --     Just s -> isSameColor square s
            --     Nothing -> False
    in List.filter filterSquare squares

isSameColor : Square -> Square -> Bool
isSameColor s1 s2 = 
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

