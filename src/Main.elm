module Main exposing (..)

import Html exposing (Html)
import Svg exposing (rect, svg, g)
import Window exposing (Size, resizes)
import QuadTree exposing (..)
import Messages exposing (..)
import QuadTreeView exposing (..)
import Initialization exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { nodes : List NodeId
    , edges : List ( NodeId, NodeId )
    , quadTree : QuadTree
    }


init : ( Model, Cmd Msg )
init =
    ( { nodes = initNodeIds
      , edges = []
      , quadTree = insertedQuadTree
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize s ->
            let
                topLeft =
                    ( 0, 0 )

                botRight =
                    ( s.width |> toFloat, s.height |> toFloat )
            in
                ( Model model.nodes model.edges (updateQtBound model.quadTree (fromCorners topLeft botRight))
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes WindowResize



-- VIEW


view : Model -> Html Msg
view model =
    svg
        [ widthSvgAttr (transformBoundingBox (getQtBound model.quadTree))
        , heightSvgAttr (transformBoundingBox (getQtBound model.quadTree))
        ]
        [ viewQuadTree model.quadTree True ]
