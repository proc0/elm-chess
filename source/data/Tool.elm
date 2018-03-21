module Data.Tool exposing (..)

import Matrix exposing (Location, loc)
import Mouse exposing (Position)
import List exposing (range, foldl, map, map2, length)
import Tuple exposing (first, second, mapFirst, mapSecond)
import Maybe.Extra exposing ((?), isJust)

import Data.Type exposing (..)

-- Global settings

boardside : List Int
boardside = range 0 7

squareSize : Int
squareSize = 54

-- Global tools
-- Shorthand map over List-like
($>>) f lx = map f lx  -- <$> in Haskell
(<<$) lx f = map f lx
infixr 2 $>>
infixr 2 <<$

-- Shorthand sequential application 
-- <*> in Haskell
($$>) lf lx = map2 (\f x -> f x) lf lx
-- <**> in Haskell
--(<$$) lx lf = map2 (|>) lf lx   
infixr 1 $$>
--infixr 1 <$$

--Shorthand map over Maybe
(<?) f mx = case mx of   
  (Just x) -> Just (f x)
  Nothing  -> Nothing
(?>) mx f = case mx of 
  Just x  -> Just (f x)
  Nothing -> Nothing
infixr 2 <?
infixr 2 ?>

(=>) = (,)

(??) a b = 
    if a
    then b
    else []

fst = first
snd = second

swap : (a,b) -> (b,a)
swap (a,b) = (b,a)

foldl1 : (a -> a -> a) -> List a -> Maybe a
foldl1 f xs =
  let
    mf x m = 
        Just (case m of
            Nothing -> x
            Just y -> f y x)
  in
    foldl mf Nothing xs

last : List a -> Maybe a
last = foldl1 (flip always)

mapBoth : (a -> b) -> (a,a) -> (b,b)
mapBoth fn tup =
  mapSecond fn <| mapFirst fn tup

liftAp : (a -> b -> c) -> List a -> List b -> List c
liftAp fn l1 l2 =
  fn $>> l1 $$> l2

tupleToList : (a,a) -> List a
tupleToList t = [fst t, snd t]

--
    
isPositive : Int -> Bool
isPositive n = (negate <| abs n) /= n 

-- Type castings

pos : Int -> Int -> Position
pos x_ y_ = {x=x_, y=y_}

toLocation : Position -> Location
toLocation p = loc p.y p.x -- loc col row

toLocations : List (Int, Int) -> List Location
toLocations = map (uncurry loc)

toPosition : (Int, Int) -> Position
toPosition (x_, y_) = {x=y_, y=x_} -- {row, col}

fromMousePosition : Position -> Position
fromMousePosition position = 
    let x = position.x // squareSize
            -- minus 56px from header
        y = (position.y-56) // squareSize
    in Position x y

toBoardPosition : Location -> Position
toBoardPosition location = 
    let p = location |> toPosition
        x = p.x * squareSize
        y = p.y * squareSize
    in Position x y

toBoardLocation : Position -> Location
toBoardLocation ps = toLocation <| fromMousePosition ps

px : Int -> String
px value = (toString value) ++ "px"

-- Nuetral type instances

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

idlePlayer : Color -> Player 
idlePlayer color =
    Player color (toString color ++ " Player") Idle []

nullPlayer : Player
nullPlayer = idlePlayer White

noMove : Move 
noMove =
    Move zeroLoc zeroLoc nullPiece Nothing False
