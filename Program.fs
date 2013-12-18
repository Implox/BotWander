module Program

open Util
open Geometry
open Spline

let p0 = CPoint (Point2 (1.0, 0.0), Vector2 (0.0, 1.0) |> Vector2.norm)
let p1 = CPoint (Point2 (0.0, 1.0), Vector2 (-1.0, 0.0) |> Vector2.norm)
let p2 = CPoint (Point2 (-1.0, 0.0), Vector2 (0.0, -1.0) |> Vector2.norm)
let p3 = CPoint (Point2 (0.0, -1.0), Vector2 (1.0, 0.0) |> Vector2.norm)

let points = [p2; p3; p0]

let curve = Curve (p0, p1, points)

curve |> Curve.length |> printfn "%A"
System.Console.ReadKey () |> ignore