module Depo.Lib exposing (..)

import Matrix exposing (Location, loc)
import Mouse exposing (Position)
import List exposing (concatMap, foldl, map)
import Tuple exposing (first, second, mapFirst, mapSecond)

import Data.Type exposing (..)

-- Global tools
-- map <$>
($>>) f xs = 
    map f xs  
(<<$) xs f = map f xs
infixr 2 $>>
infixr 2 <<$

-- ap <*>
($$>) fs xs = 
    concatMap (\f -> f $>> xs) fs
infixr 1 $$>

-- Maybe.map
(<?) f ma = 
    case ma of   
        Just a -> Just (f a)
        _  -> Nothing
infixr 2 <?
    
(?>) ma f = 
    case ma of 
        Just a -> Just (f a)
        _ -> Nothing
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


--
    
isPositive : Int -> Bool
isPositive n = (negate <| abs n) /= n 

px : Int -> String
px value = (toString value) ++ "px"
