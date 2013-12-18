module Anchor

open Util
open Geometry

type [<AbstractClass>] Anchor () =
    abstract member Position : Point2 with get
    abstract member Velocity : Vector2 with get

/// An anchor which is fixed to an absolute point in the world-space.
type AbsoluteAnchor (pos : Point2) =
    inherit Anchor ()
    
    override this.Position
        with get () = pos

    override this.Velocity
        with get () = Vector2.zero

/// An anchor which is fixed to the world-space but has a position which changes as a function of time.
type TemporalAnchor (posFunc : Time -> Point2, timeFunc : unit -> Time) =
    inherit Anchor ()

    override this.Position
        with get () = posFunc (timeFunc ())

    override this.Velocity
        with get () =
            let epsilon = 0.0001
            (posFunc (timeFunc () + epsilon) - posFunc (timeFunc ())) * (1.0 / epsilon)

/// An anchor which uses the "get" functions for the position and velocity of another entity in the world.
type SourcedAnchor (posFunc : unit -> Point2, velFunc : unit -> Vector2) =
    inherit Anchor ()

    override this.Position
        with get () = posFunc ()

    override this.Velocity
        with get () = velFunc ()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Anchor =
    /// Creates an absolute anchor at the given point.
    let absolute pos = AbsoluteAnchor (pos)

    /// Creates a temporal anchor using the given position and time functions.
    let temporal pFunc tFunc = TemporalAnchor (pFunc, tFunc)

    /// Creates a sourced anchor with the given.
    let sourced pFunc vFunc = SourcedAnchor (pFunc, vFunc)

    /// Composes two anchors.
    let compose (a1 : Anchor) (a2 : Anchor) = 
        SourcedAnchor ((fun _ -> a1.Position + a2.Position), (fun _ -> a1.Velocity + a2.Velocity))