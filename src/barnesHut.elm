-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/random.html


module Main exposing (..)

import Html exposing (Html)
import Svg exposing (rect, svg, g)
import Svg.Attributes exposing (width, viewBox)
import Window exposing (Size, resizes)
import BoundingBox exposing (BoundingBox)
import Math.Vector2 exposing (Vec2, vec2)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias NodeId =
    Int


type alias Node =
    { id : NodeId
    , mass : Mass
    , coords : Vec2
    }


type alias Mass =
    Float


type alias QtSubTrees =
    { nw : QuadTree, ne : QuadTree, se : QuadTree, sw : QuadTree }


type QuadTree
    = QtInternal { bound : BoundingBox, totalMass : Mass, subTrees : QtSubTrees }
    | QtOccupiedLeaf { bound : BoundingBox, occupant : Node }
    | QtEmptyLeaf { bound : BoundingBox }


getQtBound : QuadTree -> BoundingBox
getQtBound q =
    case q of
        QtInternal x ->
            x.bound

        QtOccupiedLeaf x ->
            x.bound

        QtEmptyLeaf x ->
            x.bound


updateSubQtBounds : QtSubTrees -> BoundingBox -> QtSubTrees
updateSubQtBounds originals newBound =
    let
        ctr =
            (BoundingBox.center newBound)
    in
        { nw = updateQtBound originals.nw (BoundingBox.fromCorners (BoundingBox.topLeft newBound) ctr)
        , ne = updateQtBound originals.ne (BoundingBox.fromCorners (BoundingBox.topRight newBound) ctr)
        , se = updateQtBound originals.se (BoundingBox.fromCorners (BoundingBox.bottomRight newBound) ctr)
        , sw = updateQtBound originals.sw (BoundingBox.fromCorners (BoundingBox.bottomLeft newBound) ctr)
        }


updateQtBound : QuadTree -> BoundingBox -> QuadTree
updateQtBound originalQt newBound =
    case originalQt of
        QtInternal x ->
            QtInternal { bound = newBound, totalMass = x.totalMass, subTrees = (updateSubQtBounds x.subTrees newBound) }

        QtOccupiedLeaf x ->
            QtOccupiedLeaf { bound = newBound, occupant = x.occupant }

        QtEmptyLeaf x ->
            QtEmptyLeaf { bound = newBound }



{- | If node x does not contain a body, put the new body n here.
   If node x is an internal node, update the center-of-mass and total mass of x. Recursively insert the body n in the appropriate quadrant.
   If node x is an external node, say containing a body named c, then there are two bodies n and c in the same region. Subdivide the region further by creating four children. Then, recursively insert both n and c into the appropriate quadrant(s). Since b and c may still end up in the same quadrant, there may be several subdivisions during a single insertion. Finally, update the center-of-mass and total mass of x.
-}


insertQt : Node -> QuadTree -> QuadTree
insertQt n qt =
    case qt of
        QtEmptyLeaf x ->
            QtOccupiedLeaf { bound = x.bound, occupant = n }

        QtInternal x ->
            QtInternal
                { totalMass = x.totalMass + n.mass
                , bound = x.bound
                , subTrees = insertIntoSubtrees n x.subTrees
                }

        QtOccupiedLeaf x ->
            QtInternal
                { bound = x.bound
                , totalMass = x.occupant.mass + n.mass
                , subTrees = insertIntoSubtrees n (updateSubQtBounds emptySubTree x.bound)
                }


emptySubTree : QtSubTrees
emptySubTree =
    { nw = emptyLeafWithNoBounds
    , ne = emptyLeafWithNoBounds
    , se = emptyLeafWithNoBounds
    , sw = emptyLeafWithNoBounds
    }


emptyLeafWithNoBounds : QuadTree
emptyLeafWithNoBounds =
    QtEmptyLeaf { bound = BoundingBox.fromPoint (vec2 0 0) }


insertIntoSubtrees : Node -> QtSubTrees -> QtSubTrees
insertIntoSubtrees n sbt =
    if BoundingBox.contains n.coords (getQtBound sbt.nw) then
        { nw = insertQt n sbt.nw, ne = sbt.ne, se = sbt.se, sw = sbt.sw }
    else if BoundingBox.contains n.coords (getQtBound sbt.ne) then
        { nw = sbt.nw, ne = insertQt n sbt.ne, se = sbt.se, sw = sbt.sw }
    else if BoundingBox.contains n.coords (getQtBound sbt.se) then
        { nw = sbt.nw, ne = sbt.ne, se = insertQt n sbt.se, sw = sbt.sw }
    else
        { nw = sbt.nw, ne = sbt.ne, se = sbt.se, sw = insertQt n sbt.sw }


type alias Model =
    { nodes : List NodeId
    , edges : List ( NodeId, NodeId )
    , quadTree : QuadTree
    }


fromCorners : ( Float, Float ) -> ( Float, Float ) -> BoundingBox
fromCorners topLeft botRight =
    BoundingBox.fromCorners (Math.Vector2.fromTuple topLeft) (Math.Vector2.fromTuple botRight)


init : ( Model, Cmd Msg )
init =
    ( { nodes = initNodeIds
      , edges = []
      , quadTree = insertedQuadTree
      }
    , Cmd.none
    )


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



-- UPDATE


type Msg
    = WindowResize Window.Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize s ->
            let
                topLeft =
                    ( 0, 0 )

                botRight =
                    ( toFloat s.width, toFloat s.height )
            in
                ( Model model.nodes model.edges (updateQtBound model.quadTree (fromCorners topLeft botRight))
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes WindowResize



-- VIEW


viewBoxSvgAttr : BoundingBox -> Svg.Attribute Msg
viewBoxSvgAttr bb =
    let
        bottomRight =
            BoundingBox.bottomRight bb

        topRight =
            BoundingBox.topRight bb

        pts =
            [ 0, 0, floor (Math.Vector2.getX bottomRight), floor (Math.Vector2.getY topRight) ]

        pointsAsString =
            String.join " " (List.map toString pts)
    in
        viewBox pointsAsString


widthSvgAttr : BoundingBox -> Svg.Attribute Msg
widthSvgAttr bb =
    let
        wdth =
            BoundingBox.width bb
    in
        width (toString wdth ++ "px")


type alias ShouldDrawBoundingBox =
    Bool


viewQuadTree : QuadTree -> ShouldDrawBoundingBox -> Svg.Svg Msg
viewQuadTree qt shouldDrawBoundingBox =
    case qt of
        QtEmptyLeaf params ->
            Svg.g
                [ Svg.Attributes.x (BoundingBox.topLeft params.bound |> Math.Vector2.getX |> toString)
                , Svg.Attributes.y (BoundingBox.topLeft params.bound |> Math.Vector2.getY |> toString)
                ]
                [ Svg.rect
                    [ Svg.Attributes.width (((BoundingBox.width params.bound) |> toString) ++ "px")
                    , Svg.Attributes.height (((BoundingBox.height params.bound) |> toString) ++ "px")
                    , Svg.Attributes.fill "white"
                    , Svg.Attributes.strokeWidth "3px"
                    , Svg.Attributes.stroke "pink"
                    , Svg.Attributes.fillOpacity "0.0"
                    ]
                    []
                ]

        QtOccupiedLeaf params ->
            Svg.g
                [ Svg.Attributes.x (BoundingBox.topLeft params.bound |> Math.Vector2.getX |> toString)
                , Svg.Attributes.y (BoundingBox.topLeft params.bound |> Math.Vector2.getY |> toString)
                ]
                [ Svg.rect
                    [ Svg.Attributes.width (BoundingBox.width params.bound |> toString)
                    , Svg.Attributes.height (BoundingBox.height params.bound |> toString)
                    , Svg.Attributes.fill "white"
                    , Svg.Attributes.strokeWidth "3px"
                    , Svg.Attributes.stroke "blue"
                    , Svg.Attributes.fillOpacity "0.0"
                    ]
                    []
                ]

        QtInternal params ->
            Svg.g
                [ Svg.Attributes.x (BoundingBox.topLeft params.bound |> Math.Vector2.getX |> toString)
                , Svg.Attributes.y (BoundingBox.topLeft params.bound |> Math.Vector2.getY |> toString)
                ]
                [ Svg.rect
                    [ Svg.Attributes.width ((BoundingBox.width params.bound) |> toString)
                    , Svg.Attributes.height ((BoundingBox.height params.bound) |> toString)
                    , Svg.Attributes.fill "white"
                    , Svg.Attributes.strokeWidth "3px"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.fillOpacity "0.0"
                    ]
                    []
                , viewQuadTree params.subTrees.nw shouldDrawBoundingBox
                , viewQuadTree params.subTrees.ne shouldDrawBoundingBox
                , viewQuadTree params.subTrees.se shouldDrawBoundingBox
                , viewQuadTree params.subTrees.sw shouldDrawBoundingBox
                ]


view : Model -> Html Msg
view model =
    svg
        [ viewBoxSvgAttr (getQtBound model.quadTree)
        , widthSvgAttr (getQtBound model.quadTree)
        ]
        [ viewQuadTree model.quadTree True ]
