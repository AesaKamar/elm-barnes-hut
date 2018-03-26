-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/random.html


module Main exposing (..)

import Html exposing (Html)


-- import Html.Events exposing (..)
-- import Random

import Svg exposing (..)
import Svg.Attributes exposing (..)


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
    }


init : ( Model, Cmd Msg )
init =
    ( { nodes = [], edges = [] }, Cmd.none )



-- UPDATE


type Msg
    = Unit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    svg [ viewBox "0 0 100 100", width "300px" ]
        [ text_ [ color "black", transform "translate(20,20)" ] [ text "Hello World" ]
        ]
