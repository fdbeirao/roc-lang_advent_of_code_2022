interface Stack
    exposes [
        Stack,
        empty,
        withCapacity,
        isEmpty,
        put,
        tryTop,
        tryPop,
    ]
    imports []

Stack a := List a

empty : {} -> Stack *
empty = \{} -> fromList []

fromList : List a -> Stack a
fromList = \list -> @Stack list

withCapacity : Nat -> Stack a
withCapacity = \elements ->
    @Stack (List.withCapacity elements)

isEmpty : Stack * -> Bool
isEmpty = \@Stack stack ->
    stack |> List.isEmpty

put : Stack a, List a -> Stack a
put = \@Stack stack, elems ->
    @Stack (stack |> List.concat elems)

tryTop : Stack a -> Result a [StackWasEmpty]
tryTop = \@Stack stack ->
    stack |> List.last |> Result.mapErr (\_ -> StackWasEmpty)

tryPop : Stack a, Nat -> Result { elems : List a, stack : Stack a } [NotEnoughItemsInStack]
tryPop = \@Stack stack, howMany ->
    stackSize = stack |> List.len

    if stackSize >= howMany then
        splitList = stack |> List.split (stackSize - howMany)
        Ok { elems: splitList.others |> List.reverse, stack: @Stack (splitList.before) }
    else
        Err NotEnoughItemsInStack

expect (empty {} |> isEmpty) == Bool.true

expect (withCapacity 2 |> isEmpty) == Bool.true

expect (empty {} |> put ["A"] |> isEmpty) == Bool.false

expect (empty {} |> tryTop) == Err StackWasEmpty

expect (empty {} |> tryPop 1 |> Result.isErr)

expect (empty {} |> put ["A"] |> tryTop) == Ok "A"

expect (empty {} |> put [1, 2] |> tryPop 1 |> Result.map .elems) == Ok [2]

expect (empty {} |> put ["A"] |> put ["B"] |> tryPop 1 |> Result.map .elems) == Ok ["B"]
expect (empty {} |> put ["A", "B"] |> tryPop 1 |> Result.map .elems) == Ok ["B"]

expect
    (
        empty {}
        |> put [1, 2]
        |> tryPop 1
        |> Result.map .stack
        |> Result.try (\s -> s |> tryPop 1)
        |> Result.map .elems
    )
    == Ok [1]

expect
    (
        empty {}
        |> put [1]
        |> put [2]
        |> tryPop 2
        |> Result.map .elems
    )
    == Ok [2, 1]

expect
    (
        empty {}
        |> put [1]
        |> put [2]
        |> tryPop 1
        |> Result.map .stack
        |> Result.try (\s -> s |> tryPop 1)
        |> Result.map .stack
        |> Result.map isEmpty
    )
    == Ok Bool.true
