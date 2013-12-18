module Util

/// Contains useful extension functions for the F# List module.
module List =
    /// Returns the first n elements in a list.
    let inline take n (lst : _ list) = [for i = 0 to n - 1 do yield lst.[i]]

    /// Drops the first n elements in a list and returns what remains.
    let inline drop n (lst : _ list) = [for i = n to lst.Length - 1 do yield lst.[i]]

    /// Takes a given collection and returns two collections that are partitioned at the given index.
    let splitAt n (lst : _ list) = (take n lst, drop n lst)

    /// Inserts a value at the given index
    let insert (lst : _ list) (index : int) value =
        let init, tail = lst |> splitAt index
        init @ (value :: tail)

type Time = double