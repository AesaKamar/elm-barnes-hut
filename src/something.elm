-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/random.html


module Main exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Window exposing (Size, resizes)


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


type alias Model =
    { nodes : List NodeId
    , edges : List ( NodeId, NodeId )
    , boundingSize : Window.Size
    }


init : ( Model, Cmd Msg )
init =
    ( { nodes = []
      , edges = []
      , boundingSize = { height = 300, width = 300 }
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = WindowResize Window.Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize s ->
            ( Model model.nodes model.edges s, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes WindowResize



-- VIEW


viewBoxSvgAttr : Window.Size -> Svg.Attribute Msg
viewBoxSvgAttr s =
    let
        points =
            [ 0, 0, s.height, s.width ]

        pointsAsString =
            String.join " " (List.map toString points)
    in
        viewBox pointsAsString


widthSvgAttr : Int -> Svg.Attribute Msg
widthSvgAttr w =
    width (toString w ++ "px")


view : Model -> Html Msg
view model =
    svg
        [ viewBoxSvgAttr model.boundingSize
        , widthSvgAttr model.boundingSize.width
        ]
        [ text_
            [ color "black"
            , transform "translate(20,20)"
            ]
            [ text "Hello World" ]
        ]
