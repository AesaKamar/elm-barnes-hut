module QuadTreeViewTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Math.Vector2 exposing (vec2)
import BoundingBox as BB


bb =
    BB.fromCorners (vec2 0 0) (vec2 100 100)


suite : Test
suite =
    describe "Helper functions"
        [ describe "Asserting about the corners of a BBox"
            [ test "topLeft should be 0 100" <|
                \_ ->
                    (BB.topLeft bb) |> Expect.equal (vec2 0 100)
            , test "bottomRight should be 100 0" <|
                \_ ->
                    (BB.bottomRight bb) |> Expect.equal (vec2 100 0)
            ]
        ]
