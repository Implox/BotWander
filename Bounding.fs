module Bounding

open Util
open Geometry
open Geometry.Rectangle
open Spline

open System

module TreeBox =
    /// Tree-type structure which acts as a bounding area for a spline.
    type T =
        | Branch of Rectangle * T * T
        | Leaf of Rectangle

    /// Creates a TreeBox for a given spline curve.
    let create (curve : Curve) = raise (NotImplementedException ())

    /// Checks if the bounding tree of a spline contains or overlaps with
    /// a given rectangle.
    let contains (tree : T) (rect : Rectangle) =
        let rec iter tree =
            match tree with
            | Branch (quad, left, right) ->
                match quad |> overlaps rect with
                | true -> (iter left) || (iter right)
                | false -> false
            | Leaf (quad) -> quad |> overlaps rect
        iter tree