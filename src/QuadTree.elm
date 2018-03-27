module QuadTree exposing (..)

import Math.Vector2 exposing (Vec2, vec2)
import BoundingBox exposing (BoundingBox, contains)


-- TYPES


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



-- FUNCTIONS


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



-- VALUES


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
    if (getQtBound sbt.nw) |> (contains <| n.coords) then
        { nw = insertQt n sbt.nw, ne = sbt.ne, se = sbt.se, sw = sbt.sw }
    else if (getQtBound sbt.ne) |> (contains <| n.coords) then
        { nw = sbt.nw, ne = insertQt n sbt.ne, se = sbt.se, sw = sbt.sw }
    else if (getQtBound sbt.se) |> (contains <| n.coords) then
        { nw = sbt.nw, ne = sbt.ne, se = insertQt n sbt.se, sw = sbt.sw }
    else
        { nw = sbt.nw, ne = sbt.ne, se = sbt.se, sw = insertQt n sbt.sw }
