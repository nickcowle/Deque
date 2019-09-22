namespace Deque.Test

open Deque
open FsCheck
open Xunit

module TestDeque =

    [<Fact>]
    let ``The empty Deque has no elements`` () =
        Assert.True (Deque.empty |> Deque.toSeq |> Seq.isEmpty)


    [<Fact>]
    let ``count returns the number of elements`` () =
        let prop d = d |> Deque.toSeq |> Seq.length = (d |> Deque.count)
        TestUtils.check prop


    [<Fact>]
    let ``enqueueLeft prepends to the elements`` () =
        let prop (d : int Deque) (i : int) =
            let d2 = d |> Deque.enqueueLeft i
            d2 |> Deque.toSeq |> Seq.toList = i :: (d |> Deque.toSeq |> Seq.toList)
        TestUtils.check prop


    [<Fact>]
    let ``enqueueRight appends to the elements`` () =
        let prop (d : int Deque) (i : int) =
            let d2 = d |> Deque.enqueueRight i
            d2 |> Deque.toSeq |> Seq.toList = (d |> Deque.toSeq |> Seq.toList) @ [i]
        TestUtils.check prop


    [<Fact>]
    let ``tryPeekLeft returns the first element`` () =
        let prop (d : int Deque) =
            d |> Deque.tryPeekLeft = (d |> Deque.toSeq |> Seq.tryHead)
        TestUtils.check prop


    [<Fact>]
    let ``tryPeekRight returns the last element`` () =
        let prop (d : int Deque) =
            d |> Deque.tryPeekRight = (d |> Deque.toSeq |> Seq.tryLast)
        TestUtils.check prop


    [<Fact>]
    let ``tryDequeueLeft removes the first element`` () =
        let prop (d : int Deque) =
            match d |> Deque.tryDequeueLeft with
            | None -> d |> Deque.count = 0
            | Some (i, d2) -> d |> Deque.toSeq |> Seq.toList = i :: (d2 |> Deque.toSeq |> Seq.toList)
        TestUtils.check prop


    [<Fact>]
    let ``tryDequeueRight removes the last element`` () =
        let prop (d : int Deque) =
            match d |> Deque.tryDequeueRight with
            | None -> d |> Deque.count = 0
            | Some (i, d2) -> d |> Deque.toSeq |> Seq.toList = (d2 |> Deque.toSeq |> Seq.toList) @ [i]
        TestUtils.check prop


    [<Fact>]
    let ``Test Deque spec`` () =

        let makeCommand (command : 'a DequeCommand) =
            { new Command<'a Deque list, 'a list list> () with
                override __.RunActual ds =
                    match ds with
                    | [] -> failwith "The list of Deques should never be empty"
                    | d::ds ->
                        let d2 = TestUtils.applyCommand d command
                        d2::d::ds
                override __.RunModel xss =
                    match xss with
                    | [] -> failwith "The list of lists should never be empty"
                    | xs::xss ->
                        match command with
                        | EnqueueLeft a -> (a :: xs)::xs::xss
                        | EnqueueRight a -> (xs @ [a])::xs::xss
                        | TryDequeueLeft ->
                            let xs2 = match xs with [] -> [] | x::xs -> xs
                            xs2::xs::xss
                        | TryDequeueRight ->
                            let xs2 = match xs with [] -> [] | _ -> xs |> List.take ((xs |> List.length) - 1)
                            xs2::xs::xss
                override __.Post(ds, xss) =
                    let elementsEqual = (ds |> List.map (Deque.toSeq >> Seq.toList)) = xss
                    elementsEqual |@ sprintf "Model: %+A, Actual: %+A" xss ds
                override __.ToString () = sprintf "%+A" command
            }

        let spec =
            { new ICommandGenerator<int Deque list, int list list> with
                member __.InitialActual = [ Deque.empty ]
                member __.InitialModel = [[]]
                member __.Next model = TestUtils.command |> Gen.map makeCommand
            }

        let config = { Config.QuickThrowOnFailure with MaxTest = 1 }
        Check.One (config, Command.toProperty spec)
