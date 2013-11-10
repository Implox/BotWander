module Program

open Util
open Geometry
open Spline

// Some testing for error in the Segment.length function
let pStart = CPoint (Point.Zero)
let pEnd = CPoint (Point (10.0, 10.0))

pStart.Dir <- (pEnd.Pos - pStart.Pos)
pEnd.Dir <- pStart.Dir

let seg = Segment.create pStart pEnd

(Segment.length seg) - sqrt (10.0 ** 2.0 * 2.0) |> printfn "%A"
System.Console.ReadKey () |> ignore