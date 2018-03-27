module Initialization exposing (..)

import BoundingBox exposing (BoundingBox)
import Math.Vector2 exposing (fromTuple, vec2)
import QuadTree exposing (..)


fromCorners : ( Float, Float ) -> ( Float, Float ) -> BoundingBox
fromCorners topLeft botRight =
    BoundingBox.fromCorners (fromTuple topLeft) (fromTuple botRight)


initNodeIds : List NodeId
initNodeIds =
    [ 1, 2, 3, 4 ]


initNodes : List Node
initNodes =
    (List.map (\i -> { id = i, mass = 1.0, coords = vec2 1.0 1.0 }) initNodeIds)


initialQuadTree : QuadTree
initialQuadTree =
    QtEmptyLeaf { bound = (fromCorners ( 0, 0 ) ( 1000, 1000 )) }


insertedQuadTree : QuadTree
insertedQuadTree =
    List.foldl (insertQt) initialQuadTree initNodes
