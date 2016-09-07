module Dag exposing
  ( Dag, Edge
  , fromList
  , shortestPath
  )

import Dict exposing (Dict)


type alias Edge a =
  { from : Int
  , to : Int
  , value : a
  }


type alias Dag a =
  Dict Int (List (Int, a))



-- CREATE


fromList : List (Edge a) -> Dag a
fromList edges =
  List.foldl addNode Dict.empty edges


addNode : Edge a -> Dag a -> Dag a
addNode { from, to, value } graph =
  let
    addNodeHelp maybeNode =
      case maybeNode of
        Nothing ->
          Just [ (to, value) ]

        Just kids ->
          Just ((to, value) :: kids)
  in
    if from < to then
      Dict.update from addNodeHelp graph

    else
      graph


-- SHORTEST PATH


shortestPath : Int -> Int -> Dag a -> Maybe (List a)
shortestPath root goal graph =
  Maybe.map .path (shortestPathHelp root goal graph)


type alias Path a =
  { length : Int
  , path : List a
  }


shortestPathHelp : Int -> Int -> Dag a -> Maybe (Path a)
shortestPathHelp root goal graph =
  if root == goal then
    Just (Path 0 [])

  else
    case Dict.get root graph of
      Nothing ->
        Nothing

      Just kids ->
        kids
          |> List.filterMap (shartestSubPath goal graph)
          |> pickShortest Nothing


shartestSubPath : Int -> Dag a -> (Int, a) -> Maybe (Path a)
shartestSubPath goal graph (root, value) =
  case shortestPathHelp root goal graph of
    Nothing ->
      Nothing

    Just { length, path } ->
      Just (Path (length + 1) (value :: path))


pickShortest : Maybe (Path a) -> List (Path a) -> Maybe (Path a)
pickShortest shortest unexplored =
  case unexplored of
    [] ->
      shortest

    contender :: rest ->
      case shortest of
        Nothing ->
          Just contender

        Just { length } ->
          if contender.length < length then
            Just contender

          else
            shortest
