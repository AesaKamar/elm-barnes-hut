module QuadTreeView exposing (..)

import BoundingBox as BB exposing (BoundingBox)
import Svg.Attributes as Atrs exposing (..)
import Svg exposing (..)
import QuadTree exposing (..)
import Messages exposing (..)
import Math.Vector2 exposing (getX, getY)


-- TYPES


type alias ShouldDrawBoundingBox =
    Bool



-- SVG FUNCTIONS


viewBoxSvgAttr : BoundingBox -> Svg.Attribute Msg
viewBoxSvgAttr bb =
    let
        pts =
            [ 0
            , 0
            , floor (getX (BB.bottomRight bb))
            , floor (getY (BB.topRight bb))
            ]

        pointsAsString =
            String.join " " (List.map toString pts)
    in
        viewBox pointsAsString


widthSvgAttr : BoundingBox -> Svg.Attribute Msg
widthSvgAttr bb =
    width (toString (BB.width bb) ++ "px")


heightSvgAttr : BoundingBox -> Svg.Attribute Msg
heightSvgAttr bb =
    height (toString (BB.height bb) ++ "px")


emptyQuadrantAttrs =
    [ Atrs.fill "white"
    , Atrs.strokeWidth "3px"
    , Atrs.stroke "pink"
    , Atrs.fillOpacity "0.0"
    ]


occupiedQuadrantAttrs =
    [ Atrs.fill "white"
    , Atrs.strokeWidth "3px"
    , Atrs.stroke "blue"
    , Atrs.fillOpacity "0.0"
    ]


subTreeAttrs =
    [ Atrs.fill "white"
    , Atrs.strokeWidth "3px"
    , Atrs.stroke "black"
    , Atrs.fillOpacity "0.0"
    ]



-- RENDERING FUNCTIONS


viewQuadTree : QuadTree -> ShouldDrawBoundingBox -> Svg.Svg Msg
viewQuadTree qt shouldDrawBoundingBox =
    case qt of
        QtEmptyLeaf params ->
            Svg.g
                [ Atrs.x (BB.topLeft params.bound |> getX |> toString)
                , Atrs.y (BB.topLeft params.bound |> getY |> toString)
                ]
                [ Svg.rect
                    ([ Atrs.width (((BB.width params.bound) |> toString) ++ "px")
                     , Atrs.height (((BB.height params.bound) |> toString) ++ "px")
                     ]
                        ++ emptyQuadrantAttrs
                    )
                    []
                ]

        QtOccupiedLeaf params ->
            Svg.g
                [ Atrs.x (BB.topLeft params.bound |> getX |> toString)
                , Atrs.y (BB.topLeft params.bound |> getY |> toString)
                ]
                [ Svg.rect
                    ([ Atrs.width (BB.width params.bound |> toString)
                     , Atrs.height (BB.height params.bound |> toString)
                     ]
                        ++ occupiedQuadrantAttrs
                    )
                    []
                ]

        QtInternal params ->
            Svg.g
                [ Atrs.x (BB.topLeft params.bound |> getX |> toString)
                , Atrs.y (BB.topLeft params.bound |> getY |> toString)
                ]
                [ Svg.rect
                    ([ Atrs.width ((BB.width params.bound) |> toString)
                     , Atrs.height ((BB.height params.bound) |> toString)
                     ]
                        ++ subTreeAttrs
                    )
                    []
                , viewQuadTree params.subTrees.nw shouldDrawBoundingBox
                , viewQuadTree params.subTrees.ne shouldDrawBoundingBox
                , viewQuadTree params.subTrees.se shouldDrawBoundingBox
                , viewQuadTree params.subTrees.sw shouldDrawBoundingBox
                ]
