app "app"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.2/tE4xS_zLdmmxmHwHih9kHWQ7fsXtJr7W7h3425-eZFk.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        "../puzzle.txt" as puzzle : Str,
        "../example.txt" as example : Str,
    ]
    provides [main] to pf

tryGetPriority : Str -> Result U8 [InvalidInput Str]
tryGetPriority = \itemType ->
    when itemType |> Str.toUtf8 is
        [utf8val] ->
            # a=97 z=122 A=65 Z=90
            if utf8val >= 97 && utf8val <= 122 then
                Ok (utf8val - 96)
            else if utf8val >= 65 && utf8val <= 90 then
                Ok (utf8val - 64 + 26)
            else
                Err (InvalidInput itemType)

        _ -> Err (InvalidInput itemType)

tryFindGroupBadge : (Str, Str, Str) -> Result Str [NoSingleCommonGraphemeFound (Str, Str, Str)]
tryFindGroupBadge = \(first, second, third) ->
    if first |> Str.isEmpty || second |> Str.isEmpty || third |> Str.isEmpty then
        Err (NoSingleCommonGraphemeFound (first, second, third))
    else
        firstGraphemes = first |> Str.graphemes |> Set.fromList
        secondGraphemes = second |> Str.graphemes |> Set.fromList
        thirdGraphemes = third |> Str.graphemes |> Set.fromList

        commonGraphemes =
            firstGraphemes
            |> Set.intersection secondGraphemes
            |> Set.intersection thirdGraphemes
            |> Set.toList

        when commonGraphemes is
            [singleCommonGrapheme] -> Ok singleCommonGrapheme
            _ -> Err (NoSingleCommonGraphemeFound (first, second, third))

chunkEvery : List a, Nat -> Result (List (List a)) [CannotChunkByZero]
chunkEvery = \list, itemsPerChunk ->
    if itemsPerChunk <= 0 then
        Err CannotChunkByZero
    else
        walkFunc = \(final, currentChunk, itemsInCurrentChunk), elem ->
            if itemsInCurrentChunk == itemsPerChunk then
                (final |> List.append currentChunk, [elem], 1)
            else
                (final, currentChunk |> List.append elem, itemsInCurrentChunk + 1)

        (chunks, lastChunk, _) = list |> List.walk ([], [], 0) walkFunc

        if lastChunk |> List.isEmpty then
            Ok chunks
        else
            Ok (chunks |> List.append lastChunk)

sumOfBadgesPerGroup : Str -> Result U32 [InvalidInput Str, NoSingleCommonGraphemeFound (Str, Str, Str), IncompleteGroupDetected]
sumOfBadgesPerGroup = \rawInput ->
    allElves =
        rawInput
        |> Str.split "\n"
        |> List.dropIf Str.isEmpty

    listOf3ToTuple = \group ->
        when (group |> List.get 0, group |> List.get 1, group |> List.get 2) is
            (Ok first, Ok second, Ok third) -> Ok (first, second, third)
            _ -> Err IncompleteGroupDetected

    if (List.len allElves) % 3 != 0 then
        Err IncompleteGroupDetected
    else
        allElves
        |> chunkEvery 3
        |> Result.mapErr (\_ -> IncompleteGroupDetected)
        |> Result.try (\groups -> groups |> List.mapTry listOf3ToTuple)
        |> Result.try (\groups -> groups |> List.mapTry tryFindGroupBadge)
        |> Result.try (\groups -> groups |> List.mapTry tryGetPriority)
        |> Result.map (\groups -> groups |> List.map Num.toU32)
        |> Result.map List.sum

main : Task {} []
main =
    prettyPrint = \label, rawInput ->
        when (rawInput |> sumOfBadgesPerGroup) is
            Ok result ->
                resultStr = result |> Num.toStr
                "\(label): result is \(resultStr)"

            Err (InvalidInput reason) ->
                "\(label): InvalidInput: [\(reason)]"

            Err (NoSingleCommonGraphemeFound (first, second, third)) ->
                "\(label): No single common grapheme found between [\(first)], [\(second)] and [\(third)]"

            Err IncompleteGroupDetected ->
                "\(label): Incomplete group detected"

    _ <- (prettyPrint "example" example) |> Stdout.line |> Task.await

    (prettyPrint "puzzle" puzzle) |> Stdout.line

expect (tryGetPriority "a") == Ok 1
expect (tryGetPriority "f") == Ok 6
expect (tryGetPriority "y") == Ok 25
expect (tryGetPriority "z") == Ok 26
expect (tryGetPriority "A") == Ok 27
expect (tryGetPriority "F") == Ok 32
expect (tryGetPriority "Y") == Ok 51
expect (tryGetPriority "Z") == Ok 52
expect (tryGetPriority "") == Err (InvalidInput "")
expect (tryGetPriority "_") == Err (InvalidInput "_")
expect (tryGetPriority "aa") == Err (InvalidInput "aa")

expect (tryFindGroupBadge ("a", "a", "a")) == Ok "a"
expect (tryFindGroupBadge ("abc", "abc", "abc")) == Err (NoSingleCommonGraphemeFound ("abc", "abc", "abc"))
expect (tryFindGroupBadge ("abc", "def", "klm")) == Err (NoSingleCommonGraphemeFound ("abc", "def", "klm"))
expect (tryFindGroupBadge ("", "", "")) == Err (NoSingleCommonGraphemeFound ("", "", ""))
expect (tryFindGroupBadge ("abcZ", "deZfh", "Zklm")) == Ok "Z"

expect ([] |> chunkEvery 1) == Ok []
expect ([] |> chunkEvery 5) == Ok []
expect ([] |> chunkEvery 0) == Err CannotChunkByZero
expect (["a", "b", "c"] |> chunkEvery 1) == Ok [["a"], ["b"], ["c"]]
expect (["a", "b", "c"] |> chunkEvery 2) == Ok [["a", "b"], ["c"]]
expect (["a", "b", "c"] |> chunkEvery 3) == Ok [["a", "b", "c"]]
expect (["a", "b", "c"] |> chunkEvery 4) == Ok [["a", "b", "c"]]

expect (sumOfBadgesPerGroup example) == Ok 70
expect (sumOfBadgesPerGroup puzzle) == Ok 2821
