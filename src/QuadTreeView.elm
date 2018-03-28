module QuadTreeView exposing (..)

import BoundingBox as BB exposing (BoundingBox)
import Svg.Attributes as Atrs
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
        Atrs.viewBox pointsAsString


widthSvgAttr : TopLeftOriginBoundingBox -> Svg.Attribute Msg
widthSvgAttr bb =
    Atrs.width (toString (bb.width) ++ "px")


heightSvgAttr : TopLeftOriginBoundingBox -> Svg.Attribute Msg
heightSvgAttr bb =
    Atrs.height (toString (bb.height) ++ "px")


displacementXAttr : TopLeftOriginBoundingBox -> Svg.Attribute Msg
displacementXAttr bb =
    Atrs.x (toString (bb.xDisplacement) ++ "px")


displacementYAttr : TopLeftOriginBoundingBox -> Svg.Attribute Msg
displacementYAttr bb =
    Atrs.y (toString (bb.yDisplacement) ++ "px")


positioningAttrs : TopLeftOriginBoundingBox -> List (Svg.Attribute Msg)
positioningAttrs bb =
    [ heightSvgAttr bb
    , widthSvgAttr bb
    , displacementXAttr bb
    , displacementYAttr bb
    ]


emptyQuadrantAttrs =
    [ Atrs.fill "none"
    , Atrs.strokeWidth "3px"
    , Atrs.stroke "pink"
    , Atrs.fillOpacity "0.0"
    ]


occupiedQuadrantAttrs =
    [ Atrs.fill "none"
    , Atrs.strokeWidth "3px"
    , Atrs.stroke "blue"
    ]


subTreeAttrs =
    [ Atrs.fill "none"
    , Atrs.stroke "none"
    ]


type alias TopLeftOriginBoundingBox =
    { xDisplacement : Float, yDisplacement : Float, height : Float, width : Float }


transformBoundingBox : BoundingBox -> TopLeftOriginBoundingBox
transformBoundingBox bb =
    { xDisplacement = (BB.bottomLeft bb) |> getX
    , yDisplacement = (BB.bottomLeft bb) |> getY
    , height = BB.height bb
    , width = BB.width bb
    }


assignBoundingBoxPositioningAttributes : BoundingBox -> List (Svg.Attribute Msg)
assignBoundingBoxPositioningAttributes bb =
    let
        translated =
            transformBoundingBox bb
    in
        [ Atrs.x (translated.xDisplacement |> toString)
        , Atrs.y (translated.yDisplacement |> toString)
        , Atrs.width (translated.width |> toString)
        , Atrs.height (translated.height |> toString)
        ]



-- RENDERING FUNCTIONS


viewQuadTree : QuadTree -> ShouldDrawBoundingBox -> Svg.Svg Msg
viewQuadTree qt shouldDrawBoundingBox =
    case qt of
        QtEmptyLeaf params ->
            Svg.rect
                (positioningAttrs (transformBoundingBox params.bound) ++ emptyQuadrantAttrs)
                []

        QtOccupiedLeaf params ->
            Svg.g
                []
                [ Svg.rect
                    (positioningAttrs (transformBoundingBox params.bound)
                        ++ occupiedQuadrantAttrs
                    )
                    []
                ]

        QtInternal params ->
            Svg.g
                []
                [ Svg.rect
                    (positioningAttrs (transformBoundingBox params.bound)
                        ++ subTreeAttrs
                    )
                    []
                , viewQuadTree params.subTrees.nw shouldDrawBoundingBox
                , viewQuadTree params.subTrees.ne shouldDrawBoundingBox
                , viewQuadTree params.subTrees.se shouldDrawBoundingBox
                , viewQuadTree params.subTrees.sw shouldDrawBoundingBox
                ]
