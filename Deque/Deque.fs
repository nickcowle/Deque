namespace Deque

[<Struct>]
type 'a DequeValues =
| Empty
| Values of values:'a array
| Nested of nested:'a DequeValues array

[<RequireQualifiedAccess>]
module DequeValues =

    let rec get (width : int) (vs : 'a DequeValues) (depth : int) (i : int) : 'a =
        match vs with
        | Empty -> failwith "Can't get from empty!"
        | Values arr -> arr.[i]
        | Nested arr ->
            let innerSize = pown width (depth - 1)
            get width arr.[i / innerSize] (depth - 1) (i % innerSize)

[<Struct>]
type 'a Deque =
    {
        Width : int
        Values : 'a DequeValues
        Depth : int
        Start : int
        Count : int
    }

[<RequireQualifiedAccess>]
module Deque =

    let private defaultWidth = 32

    let make width values depth start count =
        {
            Width = width
            Values = values
            Depth = depth
            Start = start
            Count = count
        }

    let empty<'a> : 'a Deque = make defaultWidth Empty 0 0 0

    let emptyWithCustomWidth width : 'a Deque = make width Empty 0 0 0

    let count d = d.Count

    let get d = DequeValues.get d.Width d.Values d.Depth

    let rec addSimple (width : int) (vs : 'a DequeValues) (depth : int) (index : int) (a : 'a) : 'a DequeValues =
        match vs with
        | Empty ->
            let rec make depth index =
                match depth with
                | 1 ->
                    let arr = Array.zeroCreate width
                    arr.[index] <- a
                    Values arr
                | _ ->
                    let innerSize = pown width (depth - 1)
                    let arr = Array.create width Empty
                    arr.[index / innerSize] <- make (depth - 1) (index % innerSize)
                    Nested arr
            make depth index
        | Values arr ->
            let arr = Array.copy arr
            arr.[index] <- a
            Values arr
        | Nested arr ->
            let innerSize = pown width (depth - 1)
            let vs = addSimple width arr.[index / innerSize] (depth - 1) (index % innerSize) a
            let arr = Array.copy arr
            arr.[index / innerSize] <- vs
            Nested arr

    let rec makeLast (width : int) (depth : int) (a : 'a) : 'a DequeValues =
        match depth with
        | 1 ->
            let arr = Array.zeroCreate width
            arr.[width - 1] <- a
            Values arr
        | _ ->
            let arr = Array.create width Empty
            arr.[width - 1] <- makeLast width (depth - 1) a
            Nested arr

    let rec makeFirst (width : int) (depth : int) (a : 'a) : 'a DequeValues =
        match depth with
        | 1 ->
            let arr = Array.zeroCreate width
            arr.[0] <- a
            Values arr
        | _ ->
            let arr = Array.create width Empty
            arr.[0] <- makeFirst width (depth - 1) a
            Nested arr

    let makeSingleton (width : int) (a : 'a) : 'a Deque =
        let arr = Array.zeroCreate width
        let index = width / 2
        arr.[index] <- a
        make width (Values arr) 1 index 1

    let enqueueLeft (a : 'a) (d : 'a Deque) : 'a Deque =
        match d.Depth with
        | 0 -> makeSingleton d.Width a
        | _ ->
            if d.Start <> 0 then
                let vs = addSimple d.Width d.Values d.Depth (d.Start - 1) a
                make d.Width vs d.Depth (d.Start - 1) (d.Count + 1)
            else
                let arr = Array.create d.Width Empty
                let index = d.Width / 2
                arr.[index] <- d.Values
                arr.[index - 1] <- makeLast d.Width d.Depth a
                let start = index * (pown d.Width d.Depth) - 1
                make d.Width (Nested arr) (d.Depth + 1) start (d.Count + 1)

    let enqueueRight (a : 'a) (d : 'a Deque) : 'a Deque =
        match d.Depth with
        | 0 -> makeSingleton d.Width a
        | _ ->
            if d.Start + d.Count <> pown d.Width d.Depth then
                let vs = addSimple d.Width d.Values d.Depth (d.Start + d.Count) a
                make d.Width vs d.Depth d.Start (d.Count + 1)
            else
                let arr = Array.create d.Width Empty
                let index = d.Width / 2
                arr.[index] <- d.Values
                arr.[index + 1] <- makeFirst d.Width d.Depth a
                let start = index * (pown d.Width d.Depth) + d.Start
                make d.Width (Nested arr) (d.Depth + 1) start (d.Count + 1)

    let tryPeekLeft (d : 'a Deque) : 'a option =
        match d.Count with
        | 0 -> None
        | _ -> get d d.Start |> Some

    let tryPeekRight (d : 'a Deque) : 'a option =
        match d.Count with
        | 0 -> None
        | _ -> get d (d.Start + d.Count - 1) |> Some

    let tryDequeueLeft (d : 'a Deque) : ('a * 'a Deque) option =
        match d.Count with
        | 0 -> None
        | _ ->
            // TODO - there are several optimisations that we can perform here:
            // 1) Remove our reference to the element being dequeued
            // 2) Remove DequeValues if any have now become empty
            // 3) Reduce our depth if possible
            let a = get d d.Start
            let d = make d.Width d.Values d.Depth (d.Start + 1) (d.Count - 1)
            (a, d) |> Some

    let tryDequeueRight (d : 'a Deque) : ('a * 'a Deque) option =
        match d.Count with
        | 0 -> None
        | _ ->
            // TODO - there are several optimisations that we can perform here:
            // 1) Remove our reference to the element being dequeued
            // 2) Remove DequeValues if any have now become empty
            // 3) Reduce our depth if possible
            let a = get d (d.Start + d.Count - 1)
            let d = make d.Width d.Values d.Depth d.Start (d.Count - 1)
            (a, d) |> Some

    let toSeq (d : 'a Deque) : 'a seq =
        {d.Start .. d.Start + d.Count - 1} |> Seq.map (get d)
