module Spline

open Util
open Geometry

open System.Collections.Generic

/// Represents a mutable control point on a spline curve with a position and a derivative.
type CPoint (pos : Point2, dir : Vector2) =
    let mutable pos = pos
    let mutable dir = dir

    new (pos : Point2) = CPoint (pos, Vector2.zero)

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

    /// Approximates the length of a given segment as a trapezoidal Riemann sum.
    let length (seg : T) =
        let c = parameterizeDerivative seg 
        let intervals = 1000.0
        let step = 1.0 / intervals
        let points = [0.0 .. step .. 1.0]
        let upper = 
            [for i = 1 to points.Length - 1 do
                let v = (c points.[i]).Length
                yield v * (points.[i] - points.[i-1])]
            |> List.sum

        let lower =
            [for i = 1 to points.Length - 1 do
                let v = (c points.[i-1]).Length
                yield v * (points.[i] - points.[i-1])]
            |> List.sum

        (upper + lower) / 2.0

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
    /// Applies a given function to each segment of a curve.
    /// Returns a list containing the results of the function for each segment.
    let private apply f (curve : Curve) =
        curve.Points 
        |> Seq.pairwise
        |> Seq.map (fun (p1, p2) -> Segment.create p1 p2)
        |> Seq.map f
        |> List.ofSeq

    /// Parameterizes each segment of a curve.
    let parameterize (curve : Curve) = 
        curve |> apply Segment.parameterize

    /// Parameterizes each segment of a cuve by its derivative.
    let parameterizeDerivative (curve : Curve) = 
        curve |> apply Segment.parameterizeDerivative

    /// Approximates the total length of the curve.
    let length (curve : Curve) =
        curve 
        |> apply Segment.length
        |> List.sum