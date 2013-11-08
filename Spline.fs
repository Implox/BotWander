module Spline

open Util
open Geometry

open System.Collections.Generic

/// Represents a mutable control point on a spline curve with a position and a derivative.
type CPoint (pos : Point, dir : Vector) =
    let mutable pos = pos
    let mutable dir = dir

    /// Gets or sets the position of this control point.
    member this.Pos
        with get () = pos
        and set value = pos <- value

    /// Gets or sets the derivative of this control point
    member this.Dir
        with get () = dir
        and set value = dir <- value

    member this.X = pos.X
    member this.Y = pos.Y
    member this.dX = dir.X
    member this.dY = dir.Y

module Segment =

    /// Represents single spline between two spline-points.
    type T = Segment of CPoint * CPoint

    /// Creates a spline segment from two spline-points
    let create (p1 : CPoint) (p2 : CPoint) = Segment (p1, p2)

    /// Gets the spline-points of this segment.
    let value (Segment (p1, p2)) = p1, p2
    
    /// Returns a function that uses a cubic Bezier polynomial to interpolate
    /// smoothly between the two points of this segment.
    let parameterize (seg : T) =
        let init, final = value seg

        let p0 = init.Pos
        let p1 = (init.Pos + init.Dir) / 3.0
        let p2 = (final.Pos - final.Dir) / 3.0
        let p3 = final.Pos

        let f (t : float) = 
            ((1.0 - t) ** 3.0) * p0 + 
            3.0 * ((1.0 - t) ** 2.0) * t * p1 +
            3.0 * (1.0 - t) * (t ** 2.0) * p2 +
            (t ** 3.0) * p3
        f

    /// Returns the derivative of the above parameterize function.
    let parameterizeDerivative (seg : T) =
        let init, final = value seg

        let p0 = init.Pos
        let p1 = (init.Pos + init.Dir) / 3.0
        let p2 = (final.Pos - final.Dir) / 3.0
        let p3 = final.Pos

        let f (t : float) =
            3.0 * ((1.0 - t) ** 2.0) * (p1 - p0) +
            6.0 * (1.0 - t) * t * (p2 - p1) +
            3.0 * (t ** 2.0) * (p3 - p2)
        f

/// Represents a continuous, 2D, piece-wise spline curve consisting of two or
/// more spline control points.
type Curve (first : CPoint, second : CPoint, ?tail : CPoint list) =
    let tail = defaultArg tail []
    let points = List<CPoint> ([first; second] @ tail)
    
    /// Adds a point to the end of this curve.
    member this.Add = points.Add

    /// Inserts a point to this curve at the given index.
    member this.Insert = points.Insert

    /// Removes a point from this curve.
    member this.Remove = points.Remove

    /// Gets all the points on this curve.
    member this.Points = seq (points.AsReadOnly ())

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Curve =
    /// Parameterizes each segment in a curve by a given function.
    let private parameterizeBy (f : Segment.T -> (float -> Vector)) (curve : Curve) =
        curve.Points 
        |> Seq.pairwise
        |> Seq.map (fun (p1, p2) -> Segment.create p1 p2)
        |> Seq.map f
        |> List.ofSeq

    let parameterize (curve : Curve) = 
        curve |> parameterizeBy Segment.parameterize

    let parameterizeDerivative (curve : Curve) = 
        curve |> parameterizeBy Segment.parameterizeDerivative