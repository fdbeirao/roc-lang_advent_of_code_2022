app "app"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.2/tE4xS_zLdmmxmHwHih9kHWQ7fsXtJr7W7h3425-eZFk.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task, await },
        "../input.txt" as input : Str,
        "../example.txt" as example : Str,
    ]
    provides [main] to pf

Range : { start : U16, end : U16 }

isFullyContainedWithin : (Range, Range) -> Bool
isFullyContainedWithin = \(small, large) ->
    small.start >= large.start && small.end <= large.end

isEitherFullyContainedInTheOther : (Range, Range) -> Bool
isEitherFullyContainedInTheOther = \(a, b) ->
    isFullyContainedWithin (a, b) || isFullyContainedWithin (b, a)

tryParseAsRange : Str -> Result Range [InvalidRangeInput Str]
tryParseAsRange = \rawInput ->
    invalidInput = Err (InvalidRangeInput rawInput)

    when (rawInput |> Str.split "-") is
        [rawStart, rawEnd] ->
            when (rawStart |> Str.toU16, rawEnd |> Str.toU16) is
                (Ok start, Ok end) ->
                    if start <= end then
                        Ok { start, end }
                    else
                        invalidInput

                _ -> invalidInput

        _ -> invalidInput

tryParseInputLine : Str -> Result (Range, Range) [InvalidInputLine Str, InvalidRangeInput Str]
tryParseInputLine = \rawInputLine ->
    invalidInputLine = Err (InvalidInputLine rawInputLine)
    when (rawInputLine |> Str.split ",") is
        [rawLeft, rawRight] ->
            when (rawLeft |> tryParseAsRange, rawRight |> tryParseAsRange) is
                (Ok left, Ok right) -> Ok (left, right)
                (Err reason, _) -> Err reason
                (_, Err reason) -> Err reason

        _ -> invalidInputLine

howManyPairsFullyContainTheOther : Str -> Result Nat [InvalidInputLine Str, InvalidRangeInput Str]
howManyPairsFullyContainTheOther = \rawInput ->
    rawInput
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.mapTry tryParseInputLine
    |> Result.map (\lines -> lines |> List.countIf isEitherFullyContainedInTheOther)

main : Task {} []
main =
    prettyPrint = \label, rawInput ->
        when (rawInput |> howManyPairsFullyContainTheOther) is
            Ok result ->
                resultStr = result |> Num.toStr
                "\(label): result is \(resultStr)"

            Err (InvalidInputLine reason) ->
                "\(label): Invalid line syntax: [\(reason)]"

            Err (InvalidRangeInput reason) ->
                "\(label): Invalid range syntax: [\(reason)]"

    _ <- (prettyPrint "example" example) |> Stdout.line |> await

    (prettyPrint "input" input) |> Stdout.line

expect (tryParseAsRange "2-4") == Ok { start: 2, end: 4 }
expect (tryParseAsRange "6-6") == Ok { start: 6, end: 6 }
expect (tryParseAsRange "0-120") == Ok { start: 0, end: 120 }
expect (tryParseAsRange "6-2") == Err (InvalidRangeInput "6-2")
expect (tryParseAsRange "6-") == Err (InvalidRangeInput "6-")
expect (tryParseAsRange "-6") == Err (InvalidRangeInput "-6")
expect (tryParseAsRange "2--6") == Err (InvalidRangeInput "2--6")

expect (tryParseInputLine "2-4,6-8") == Ok ({ start: 2, end: 4 }, { start: 6, end: 8 })
expect (tryParseInputLine "") == Err (InvalidInputLine "")
expect (tryParseInputLine "2-4;6-8") == Err (InvalidInputLine "2-4;6-8")
expect (tryParseInputLine "2-4,-8") == Err (InvalidRangeInput "-8")
expect (tryParseInputLine "-4,2-8") == Err (InvalidRangeInput "-4")
expect (tryParseInputLine "1-,-8") == Err (InvalidRangeInput "1-")

# .234.....  2-4 ❌
# .....678.  6-8
expect (isFullyContainedWithin ({ start: 2, end: 4 }, { start: 6, end: 8 })) == Bool.false

# .23......  2-3 ❌
# ...45....  4-5
expect (isFullyContainedWithin ({ start: 2, end: 3 }, { start: 4, end: 5 })) == Bool.false

# ....567..  5-7 ❌
# ......789  7-9
expect (isFullyContainedWithin ({ start: 5, end: 7 }, { start: 7, end: 9 })) == Bool.false

# .2345678.  2-8 ❌
# ..34567..  3-7
expect (isFullyContainedWithin ({ start: 2, end: 8 }, { start: 3, end: 7 })) == Bool.false

# ..34567..  3-7 ✅
# .2345678.  2-8
expect (isFullyContainedWithin ({ start: 3, end: 7 }, { start: 2, end: 8 })) == Bool.true

# .....6...  6-6 ✅
# ...456...  4-6
expect (isFullyContainedWithin ({ start: 6, end: 6 }, { start: 4, end: 6 })) == Bool.true

expect (howManyPairsFullyContainTheOther example) == Ok 2
expect (howManyPairsFullyContainTheOther input) == Ok 441
