namespace Deque.Test

open Deque
open FsCheck

type 'a DequeCommand =
| EnqueueLeft of 'a
| EnqueueRight of 'a
| TryDequeueLeft
| TryDequeueRight

[<RequireQualifiedAccess>]
module TestUtils =

    type Marker = Marker

    let command<'a> : Gen<'a DequeCommand> =
        let enqueueLeft = Arb.generate<'a> |> Gen.map EnqueueLeft
        let enqueueRight = Arb.generate<'a> |> Gen.map EnqueueRight
        let tryDequeueLeft = Gen.constant TryDequeueLeft
        let tryDequeueRight = Gen.constant TryDequeueRight
        Gen.frequency
            [
                2, enqueueLeft
                2, enqueueRight
                1, tryDequeueLeft
                1, tryDequeueRight
            ]

    let applyCommand d =
        function
        | EnqueueLeft a -> d |> Deque.enqueueLeft a
        | EnqueueRight a -> d |> Deque.enqueueRight a
        | TryDequeueLeft -> match d |> Deque.tryDequeueLeft with None -> d | Some (_, d) -> d
        | TryDequeueRight -> match d |> Deque.tryDequeueRight with None -> d | Some (_, d) -> d

    let deque<'a> : Arbitrary<'a Deque> =
        let width = Gen.choose (3, 1024)
        let makeDeque commands width = List.fold applyCommand (Deque.emptyWithCustomWidth width) commands
        Gen.apply (command |> Gen.listOf |> Gen.map makeDeque) width |> Arb.fromGen

    let config = { Config.QuickThrowOnFailure with Arbitrary = [ typeof<Marker>.DeclaringType ] ; MaxTest = 100_000 }

    let check prop = Check.One(config, prop)
