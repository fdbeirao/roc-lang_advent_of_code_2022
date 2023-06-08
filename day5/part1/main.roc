app "app"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.2/tE4xS_zLdmmxmHwHih9kHWQ7fsXtJr7W7h3425-eZFk.tar.br",
        stack: "../package/Stack/main.roc",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        "../input.txt" as input : Str,
        "../example.txt" as example : Str,
        stack.Stack.{ Stack },
    ]
    provides [main] to pf

Crate : Str

StackId : Nat

Stacks : Dict StackId (Stack Crate)

Move : { howMany : Nat, from : StackId, to : StackId }

PutCrate : (StackId, Crate)

emptyStacks = Dict.empty {}

addToStack : Stacks, Nat, List Crate -> Stacks
addToStack = \stacks, index, crates ->
    upsertFunc = \existingStack ->
        when existingStack is
            Missing -> Present (Stack.empty {} |> Stack.put crates)
            Present stack -> Present (stack |> Stack.put crates)
    stacks |> Dict.update index upsertFunc

# Given a list of stacks, output the topmost crate on each stack
#     [D]
# [N] [C]
# [Z] [M] [P]
# Would output "NDP"
readCratesOnTopOfEachStack : Stacks -> Str
readCratesOnTopOfEachStack = \stacks ->
    rawStackKeys = stacks |> Dict.keys

    stackKeys =
        when (rawStackKeys |> List.min, rawStackKeys |> List.max) is
            (Ok min, Ok max) -> List.range { start: At min, end: At max }
            _ -> []

    stackKeys
    |> List.walk
        (stacks |> Dict.len |> Str.withCapacity)
        (\state, key ->
            when stacks |> Dict.get key is
                Ok stack ->
                    when (stack |> Stack.tryTop) is
                        Ok crate -> state |> Str.concat crate
                        Err StackWasEmpty -> state |> Str.concat " "

                Err KeyNotFound -> state |> Str.concat " "
        )

# Given a list of stacks, try apply a move
#
# [D]
# [N] [C]
# [Z] [M] [P]
#
# move 3 from 1 to 3
#
# Would output an Ok Stacks with the following configuration
#         [Z]
#         [N]
#     [C] [D]
#     [M] [P]
tryApplyMove : Stacks, Move -> Result Stacks [NotEnoughCratesInOriginStack]
tryApplyMove = \stacks, move ->
    if move.howMany <= 0 then
        Ok stacks
    else
        stacks
        |> Dict.get move.from
        |> Result.try
            (\stack ->
                stack
                |> Stack.tryPop move.howMany
                |> Result.map
                    (\{ elems: crates, stack: newStack } ->
                        stacks
                        |> Dict.insert move.from newStack
                        |> addToStack move.to crates)
            )
        |> Result.mapErr (\_ -> NotEnoughCratesInOriginStack)

# Split the raw input into its two constituents: the "header" (rawStackLines) and the moves
#
#     [D]
# [N] [C]
# [Z] [M] [P]
#  1   2   3
#
# move 1 from 2 to 1
#
# Would return Ok {rawStackLines: ["    [D]    ", "[N] [C]    ", "[Z] [M] [P]"], rawMoveLines: ["move 1 from 2 to 1"]}
trySplitInput : Str -> Result { rawStackLines : List Str, rawMoveLines : List Str } [InvalidInput]
trySplitInput = \rawInput ->
    when rawInput |> Str.split "\n\n" is
        [rawStackLines, rawMoveLines] ->
            Ok {
                rawStackLines: rawStackLines |> Str.split "\n" |> List.dropIf Str.isEmpty |> List.dropLast,
                rawMoveLines: rawMoveLines |> Str.split "\n" |> List.dropIf Str.isEmpty,
            }

        _ -> Err InvalidInput

# Given a raw initial stack line from the header, parse it into a List of PutCrate operations
# "    [D]    " -> Ok [ (2, "D") ]
# "[N] [C]    " -> Ok [ (1, "N"), (2, "C") ]
# "[Z] [M] [P]" -> Ok [ (1, "Z"), (2, "M"), (3, "P") ]
tryParseHeaderStackLine : Str -> Result (List PutCrate) [InvalidHeaderLine Str]
tryParseHeaderStackLine = \headerRow ->
    walkFunc = \(operations, currentStackId, cursorPosition), grapheme ->
        when (grapheme, cursorPosition) is
            (" ", 3) -> Ok (operations, currentStackId + 1, 0)
            (" ", _) -> Ok (operations, currentStackId, cursorPosition + 1)
            ("[", 0) -> Ok (operations, currentStackId + 1, 1)
            ("]", 2) -> Ok (operations, currentStackId, -1)
            (crate, 1) -> Ok (operations |> List.append (currentStackId, crate), currentStackId, 2)
            _ -> Err (InvalidHeaderLine headerRow)

    headerRow
    |> Str.graphemes
    |> List.walkTry ([], 0, 0) walkFunc
    |> Result.map (\(operations, _, _) -> operations)

# Given a raw move line from the input, parse it into a Move operation
# "move 1 from 2 to 1" -> Ok { howMany: 1, from: 2, to: 1 }
# "move 3 from 1 to 3" -> Ok { howMany: 3, from: 1, to: 3 }
tryParseMoveRow : Str -> Result Move [InvalidMoveLine Str]
tryParseMoveRow = \rawMoveRow ->
    errResult = Err (InvalidMoveLine rawMoveRow)

    when rawMoveRow |> Str.split " " is
        ["move", rawHowMany, "from", rawFrom, "to", rawTo] ->
            when (rawHowMany |> Str.toNat, rawFrom |> Str.toNat, rawTo |> Str.toNat) is
                (Ok howMany, Ok from, Ok to) -> Ok { howMany, from, to }
                _ -> errResult

        _ -> errResult

# Given the list of put crates operation from tryParseHeaderStackLine, initializes a Stacks
getInitialStacksConfig : List (List PutCrate) -> Stacks
getInitialStacksConfig = \allPutCrates ->
    allCratesWalkFunc = \stacks, putCrates ->
        putCratesWalkFunc = \s, (stackIndex, crateId) ->
            s |> addToStack stackIndex [crateId]
        putCrates |> List.walk stacks putCratesWalkFunc

    allPutCrates
    |> List.walkBackwards
        emptyStacks
        allCratesWalkFunc

# Given a Stacks and a list of moves, try to apply these moves to create a new resulting Stacks
tryApplyMoves : Stacks, List Move -> Result Stacks [NotEnoughCratesInOriginStack]
tryApplyMoves = \stacks, moves ->
    moves |> List.walkTry stacks tryApplyMove

# All together now
trySolvePuzzle : Str -> Result Str [InvalidInput, InvalidHeaderLine Str, InvalidMoveLine Str, NotEnoughCratesInOriginStack]
trySolvePuzzle = \rawInput ->
    rawInput
    |> trySplitInput
    |> Result.try
        (\{ rawStackLines, rawMoveLines } ->
            when (rawStackLines |> List.mapTry tryParseHeaderStackLine, rawMoveLines |> List.mapTry tryParseMoveRow) is
                (Ok putCrateOperations, Ok moveOperations) ->
                    putCrateOperations
                    |> getInitialStacksConfig
                    |> tryApplyMoves moveOperations
                    |> Result.map readCratesOnTopOfEachStack

                (Err reason, _) -> Err reason
                (_, Err reason) -> Err reason
        )

main : Task {} []
main =
    prettyPrint = \label, rawInput ->
        when (rawInput |> trySolvePuzzle) is
            Ok result ->
                "\(label): result is \(result)"

            Err InvalidInput ->
                "\(label): Invalid input detected"

            Err (InvalidHeaderLine reason) ->
                "\(label): Invalid header line: [\(reason)]"

            Err (InvalidMoveLine reason) ->
                "\(label): Invalid move line: [\(reason)]"

            Err NotEnoughCratesInOriginStack ->
                "\(label): Reached a condition where there were not enough crates in the origin stack"

    _ <- (prettyPrint "example" example) |> Stdout.line |> Task.await

    (prettyPrint "input" input) |> Stdout.line

expect (emptyStacks |> addToStack 0 ["A", "B"] |> readCratesOnTopOfEachStack) == "B"
expect (emptyStacks |> addToStack 0 ["A"] |> addToStack 0 ["B"] |> readCratesOnTopOfEachStack) == "B"
expect (emptyStacks |> addToStack 0 ["A"] |> addToStack 1 ["B"] |> readCratesOnTopOfEachStack) == "AB"
expect (emptyStacks |> addToStack 0 ["A"] |> addToStack 2 ["C"] |> readCratesOnTopOfEachStack) == "A C"

# Applying a zero crate move is essentially a no-op
expect (emptyStacks |> tryApplyMove { howMany: 0, from: 0, to: 0 } |> Result.isOk)

expect
    (
        emptyStacks
        |> addToStack 0 ["A", "B"]
        |> tryApplyMove { howMany: 1, from: 0, to: 1 }
        |> Result.map readCratesOnTopOfEachStack
    )
    == Ok "AB"

expect
    (
        emptyStacks
        |> addToStack 0 ["A", "B"]
        |> tryApplyMove { howMany: 2, from: 0, to: 1 }
        |> Result.map readCratesOnTopOfEachStack
    )
    == Ok " A"

expect
    (
        emptyStacks
        |> addToStack 0 ["A", "B"]
        |> tryApplyMove { howMany: 3, from: 0, to: 1 }
        |> Result.map readCratesOnTopOfEachStack
    )
    == Err NotEnoughCratesInOriginStack

expect
    (
        emptyStacks
        |> addToStack 1 ["Z", "N"]
        |> addToStack 2 ["M", "C", "D"]
        |> addToStack 3 ["P"]
        |> tryApplyMove { howMany: 1, from: 2, to: 1 }
        |> Result.try (\s -> s |> tryApplyMove { howMany: 3, from: 1, to: 3 })
        |> Result.try (\s -> s |> tryApplyMove { howMany: 2, from: 2, to: 1 })
        |> Result.try (\s -> s |> tryApplyMove { howMany: 1, from: 1, to: 2 })
        |> Result.map readCratesOnTopOfEachStack
    )
    == Ok "CMZ"

expect (tryParseHeaderStackLine "[A]") == Ok [(1, "A")]
expect (tryParseHeaderStackLine "    [B]") == Ok [(2, "B")]
expect (tryParseHeaderStackLine "[A] [B]") == Ok [(1, "A"), (2, "B")]
expect (tryParseHeaderStackLine "[A]     [C]") == Ok [(1, "A"), (3, "C")]
expect (tryParseHeaderStackLine " [A]") == Err (InvalidHeaderLine " [A]")

expect (tryParseMoveRow "move 1 from 2 to 1") == Ok { howMany: 1, from: 2, to: 1 }
expect (tryParseMoveRow "move 50 from 20 to 100") == Ok { howMany: 50, from: 20, to: 100 }
expect (tryParseMoveRow "move 💩 from 2 to 1") == Err (InvalidMoveLine "move 💩 from 2 to 1")
expect (tryParseMoveRow "move -1 from 2 to 1") == Err (InvalidMoveLine "move -1 from 2 to 1")
expect (tryParseMoveRow "move 1 from -2 to 1") == Err (InvalidMoveLine "move 1 from -2 to 1")
expect (tryParseMoveRow "move 1 from 2 to -1") == Err (InvalidMoveLine "move 1 from 2 to -1")

expect
    (trySplitInput "[A]\n[B]\n 1 \n\nmove 1 from 2 to 1\nmove 20 from 10 to 15\n")
    ==
    Ok {
        rawStackLines: ["[A]", "[B]"],
        rawMoveLines: ["move 1 from 2 to 1", "move 20 from 10 to 15"],
    }

# [A]     [C]
# [D] [B] [E]
expect
    (
        getInitialStacksConfig [
            [(1, "A"), (3, "C")],
            [(1, "D"), (2, "B"), (3, "E")],
        ]
        |> readCratesOnTopOfEachStack
    )
    == "ABC"

expect (trySolvePuzzle example) == Ok "CMZ"
expect (trySolvePuzzle input) == Ok "DHBJQJCCW"
