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

trySplitInHalf : Str -> Result { left : Str, right : Str } [InvalidInput Str]
trySplitInHalf = \rucksackContents ->
    graphemes = rucksackContents |> Str.graphemes
    graphemesCount = graphemes |> List.len

    if (graphemes |> List.isEmpty) || (graphemesCount % 2 != 0) then
        Err (InvalidInput rucksackContents)
    else
        Ok
            (
                graphemes
                |> List.split (graphemesCount // 2)
                |> \{ before, others } -> {
                    left: before |> Str.joinWith "",
                    right: others |> Str.joinWith "",
                }
            )

findCommonItemTypes : { left : Str, right : Str } -> Set Str
findCommonItemTypes = \{ left, right } ->
    leftGraphemes = Str.graphemes left |> Set.fromList
    rightGraphemes = Str.graphemes right |> Set.fromList
    leftGraphemes |> Set.intersection rightGraphemes

sumOfAllPriorities : Str -> Result U32 [InvalidInput Str]
sumOfAllPriorities = \rawInput ->
    rawInput
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.mapTry trySplitInHalf
    |> Result.try
        (\listOfRucksackContents ->
            listOfRucksackContents
            |> List.map findCommonItemTypes
            |> List.joinMap Set.toList
            |> List.mapTry tryGetPriority
            |> Result.map (\l -> l |> List.map Num.toU32)
            |> Result.map (List.sum)
        )

main : Task {} []
main =
    prettyPrint = \label, rawInput ->
        when (rawInput |> sumOfAllPriorities) is
            Ok result ->
                resultStr = result |> Num.toStr
                "\(label): result is \(resultStr)"

            Err (InvalidInput reason) ->
                "\(label): InvalidInput: [\(reason)]"

    _ <- (prettyPrint "example" example) |> Stdout.line |> await

    (prettyPrint "input" input) |> Stdout.line

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

expect (trySplitInHalf "aa") == Ok ({ left: "a", right: "a" })
expect (trySplitInHalf "abcABC") == Ok ({ left: "abc", right: "ABC" })
expect (trySplitInHalf "") == Err (InvalidInput "")
expect (trySplitInHalf "a") == Err (InvalidInput "a")

expect (trySplitInHalf "aa" |> Result.map findCommonItemTypes) == Ok (["a"] |> Set.fromList)
expect (trySplitInHalf "aA" |> Result.map findCommonItemTypes) == Ok ([] |> Set.fromList)
expect (trySplitInHalf "vJrwpWtwJgWrhcsFMMfFFhFp" |> Result.map findCommonItemTypes) == Ok (["p"] |> Set.fromList)
expect (trySplitInHalf "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL" |> Result.map findCommonItemTypes) == Ok (["L"] |> Set.fromList)
expect (trySplitInHalf "PmmdzqPrVvPwwTWBwg" |> Result.map findCommonItemTypes) == Ok (["P"] |> Set.fromList)
expect (trySplitInHalf "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn" |> Result.map findCommonItemTypes) == Ok (["v"] |> Set.fromList)

expect (sumOfAllPriorities example) == Ok 157
expect (sumOfAllPriorities input) == Ok 8233
