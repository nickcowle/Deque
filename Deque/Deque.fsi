namespace Deque

[<Struct>]
type 'a DequeValues =
| Empty
| Values of values:'a array
| Nested of nested:'a DequeValues array

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

    val empty<'a> : 'a Deque

    val count : 'a Deque -> int

    val enqueueLeft : 'a -> 'a Deque -> 'a Deque

    val enqueueRight : 'a -> 'a Deque -> 'a Deque

    val tryPeekLeft : 'a Deque -> 'a option

    val tryPeekRight : 'a Deque -> 'a option

    val tryDequeueLeft : 'a Deque -> ('a * 'a Deque) option

    val tryDequeueRight : 'a Deque -> ('a * 'a Deque) option

    val toSeq : 'a Deque -> 'a seq

    val emptyWithCustomWidth : int -> 'a Deque
