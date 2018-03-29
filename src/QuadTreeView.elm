module QuadTreeView exposing (..)

import BoundingBox as BB exposing (BoundingBox)
import Svg.Attributes as Atrs
import Html.Attributes as HtAtrs
import Svg exposing (..)
import QuadTree exposing (..)
import Messages exposing (..)
import Math.Vector2 exposing (getX, getY, vec2, Vec2)
import Window exposing (Size)


-- TYPES


type alias ShouldDrawBoundingBox =
    Bool



-- SVG FUNCTIONS


viewBoxSvgAttr : Window.Size -> Svg.Attribute Msg
viewBoxSvgAttr bb =
    let
        pts =
            [ 0
            , 0
            , bb.width
            , bb.height
            ]

        pointsAsString =
            String.join " " (List.map toString pts)
    in
        Atrs.viewBox pointsAsString


widthSvgAttr : BoundingBox -> Svg.Attribute Msg
widthSvgAttr bb =
    Atrs.width (toString (bb |> BB.width) ++ "px")


heightSvgAttr : BoundingBox -> Svg.Attribute Msg
heightSvgAttr bb =
    Atrs.height (toString (bb |> BB.height) ++ "px")


displacementXAttr : BoundingBox -> Svg.Attribute Msg
displacementXAttr bb =
    Atrs.x (toString (bb |> BB.bottomLeft |> getX) ++ "px")


displacementYAttr : BoundingBox -> Svg.Attribute Msg
displacementYAttr bb =
    Atrs.y (toString (bb |> BB.bottomLeft |> getY) ++ "px")


positioningAttrs : BoundingBox -> List (Svg.Attribute Msg)
positioningAttrs bb =
    [ heightSvgAttr bb
    , widthSvgAttr bb
    , displacementXAttr bb
    , displacementYAttr bb
    ]


emptyQuadrantAttrs =
    [ Atrs.fill "none"
    , HtAtrs.style
        [ ( "outline-color", "pink" )
        , ( "outline-style", "solid" )
        , ( "outline-width", "4px" )
        , ( "outline-offset", "-5px" )
        ]
    , Atrs.fillOpacity "0.0"
    ]


occupiedQuadrantAttrs =
    [ Atrs.fill "none"
    , HtAtrs.style
        [ ( "outline-color", "red" )
        , ( "outline-style", "solid" )
        , ( "outline-width", "4px" )
        , ( "outline-offset", "-5px" )
        ]
    ]


subTreeAttrs =
    [ Atrs.fill "none"
    , Atrs.stroke "none"
    ]


nodeAttrs =
    [ Atrs.fill "black"
    , Atrs.stroke "none"
    , Atrs.r "5px"
    ]


nodePositioningAttrs : Window.Size -> Node -> List (Svg.Attribute Msg)
nodePositioningAttrs screenDims n =
    [ Atrs.cx (((getX n.coords) |> toString) ++ "px")
    , Atrs.cy (((getY n.coords) |> toString) ++ "px")
    ]



-- RENDERING FUNCTIONS


viewQuadTree : QuadTree -> ShouldDrawBoundingBox -> Window.Size -> Svg.Svg Msg
viewQuadTree qt shouldDrawBoundingBox screenDims =
    case qt of
        QtEmptyLeaf params ->
            Svg.rect
                (positioningAttrs params.bound ++ emptyQuadrantAttrs)
                []

        QtOccupiedLeaf params ->
            Svg.g
                []
                [ Svg.rect
                    (positioningAttrs params.bound
                        ++ occupiedQuadrantAttrs
                    )
                    []
                , Svg.circle
                    (nodeAttrs ++ (nodePositioningAttrs screenDims params.occupant))
                    []
                ]

        QtInternal params ->
            Svg.g
                []
                [ Svg.rect
                    (positioningAttrs params.bound
                        ++ subTreeAttrs
                    )
                    []
                , viewQuadTree params.subTrees.nw shouldDrawBoundingBox screenDims
                , viewQuadTree params.subTrees.ne shouldDrawBoundingBox screenDims
                , viewQuadTree params.subTrees.se shouldDrawBoundingBox screenDims
                , viewQuadTree params.subTrees.sw shouldDrawBoundingBox screenDims
                ]
