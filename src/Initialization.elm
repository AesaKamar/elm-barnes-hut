module Initialization exposing (..)

import BoundingBox exposing (BoundingBox)
import Math.Vector2 exposing (fromTuple, vec2)
import QuadTree exposing (..)


fromCorners : ( Float, Float ) -> ( Float, Float ) -> BoundingBox
fromCorners topLeft botRight =
    BoundingBox.fromCorners (fromTuple topLeft) (fromTuple botRight)


initNodeIds : List NodeId
initNodeIds =
    [ 1, 2 ]


initNodes : List Node
initNodes =
    [ { id = 1, mass = 1.0, coords = vec2 300.0 300.0 }
    , { id = 2, mass = 1.0, coords = vec2 500.0 500.0 }

    -- , { id = 3, mass = 1.0, coords = vec2 900.0 900.0 }
    ]



-- insertedQuadTree : QuadTree
-- insertedQuadTree =
--     List.foldl (insertIntoQt) initialQuadTree initNodes
