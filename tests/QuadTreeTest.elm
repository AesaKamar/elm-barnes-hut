module QuadTreeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import QuadTree exposing (..)
import Math.Vector2 exposing (vec2)
import BoundingBox as BB


suite : Test
suite =
    describe "QuadTree"
        [ describe "insertIntoQt"
            [ test "when inserting into an empty QT, should do give back an occupied leaf" <|
                \_ ->
                    let
                        n =
                            { id = 0
                            , mass = 0
                            , coords = vec2 50 50
                            }

                        bb =
                            { bound = BB.fromCorners (vec2 0 0) (vec2 100 100) }

                        inserted =
                            insertIntoQt n (QtEmptyLeaf bb)
                    in
                        case inserted of
                            QtOccupiedLeaf _ ->
                                Expect.pass

                            _ ->
                                Expect.fail (toString inserted)
            , test "when inserting into an empty subtree, should insert into the correct subTree" <|
                \_ ->
                    let
                        node =
                            { id = 0
                            , mass = 0
                            , coords = vec2 75 75
                            }

                        bb =
                            { bound = BB.fromCorners (vec2 0 0) (vec2 100 100) }

                        inserted =
                            insertIntoQt node
                                (QtInternal
                                    { totalMass = 0.0
                                    , bound = bb.bound
                                    , subTrees = emptySubTree bb.bound
                                    }
                                )
                    in
                        case inserted of
                            QtInternal params ->
                                case params.subTrees.ne of
                                    QtOccupiedLeaf x ->
                                        Expect.pass

                                    _ ->
                                        Expect.fail (toString params.subTrees)

                            _ ->
                                Expect.fail "not internal node"
            ]
        , describe "insertIntoSubQt"
            [ test "should do something" <|
                \_ ->
                    Expect.pass
            ]
        , describe "clamping functions"
            [ describe "clamp1D"
                [ test "right of range" <| \_ -> (clamp1D 1 5 6) |> Expect.equal 5
                , test "left of range" <| \_ -> (clamp1D 1 5 -12) |> Expect.equal 1
                , test "within range" <| \_ -> (clamp1D 1 5 2) |> Expect.equal 2
                ]
            , describe "clamp2d"
                [ test "should be a corner when outside of a square" <|
                    \_ ->
                        let
                            bb =
                                BB.fromCorners (vec2 0 0) (vec2 10 10)

                            point =
                                vec2 11 11

                            clamped =
                                clamp2D bb point
                        in
                            Expect.equal clamped (BB.topRight bb)
                ]
            ]
        ]



-- Nothing
