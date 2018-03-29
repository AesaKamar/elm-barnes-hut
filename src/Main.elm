module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg exposing (rect, svg, g)
import Window exposing (Size, resizes)
import QuadTree exposing (..)
import Messages exposing (..)
import QuadTreeView exposing (..)
import Initialization exposing (..)
import Svg.Attributes


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
    , screenDims : Size
    }


init : ( Model, Cmd Msg )
init =
    ( { nodes = initNodeIds
      , edges = []
      , quadTree = insertedQuadTree
      , screenDims = { height = 0, width = 0 }
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize s ->
            let
                botLeft =
                    ( 0, 0 )

                topRight =
                    ( s.width |> toFloat, s.height |> toFloat )
            in
                ( Model model.nodes model.edges (updateQtBound model.quadTree (fromCorners botLeft topRight)) s
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
        [ Svg.Attributes.width ((model.screenDims.width |> toString) ++ "px")
        , Svg.Attributes.height ((model.screenDims.height |> toString) ++ "px")
        , viewBoxSvgAttr model.screenDims
        , Html.Attributes.style [ ( "outline-offset", "-10px" ) ]
        ]
        [ viewQuadTree model.quadTree True model.screenDims ]
