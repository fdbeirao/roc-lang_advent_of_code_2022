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

main : Task {} []
main =
    prettyRes = \label, str ->
        res = str |> tryGetCaloriesCarriedByElfWithMost

        when res is
            Ok output -> output |> Num.toStr |> \calories -> "\(label): the total calories is \(calories)"
            Err ListWasEmpty -> "\(label): there was an error (list was empty)"
            Err (UnableToParseAsU32 num) -> "\(label): there was an error (unable to parse [\(num)] as U32)"

    _ <- (prettyRes "example" example) |> Stdout.line |> await

    (prettyRes "input" input) |> Stdout.line

tryGetCaloriesCarriedByElfWithMost : Str -> Result U32 [ListWasEmpty, UnableToParseAsU32 Str]
tryGetCaloriesCarriedByElfWithMost = \rawInput ->
    allLines = Str.split rawInput "\n"

    allLines
    |> splitByEmptyString # Split the List of Str into List of Lists of Str
    |> tryParseToU32 # Parse each list
    |> Result.map (\l -> l |> List.map List.sum) # Add all numbers together. This now becomes a Result (List U32)
    |> Result.try (\l -> l |> List.max)

splitByEmptyString : List Str -> List (List Str)
splitByEmptyString = \list ->
    (lists, lastList) =
        list
        |> List.walk
            ([], [])
            (\(prevLists, currentList), elem ->
                if Str.isEmpty elem then
                    (prevLists |> List.append currentList, [])
                else
                    (prevLists, currentList |> List.append elem)
            )
    lists
    |> List.append lastList
    |> List.dropIf List.isEmpty

tryParseToU32 : List (List Str) -> Result (List (List U32)) [UnableToParseAsU32 Str]
tryParseToU32 = \list ->
    innerWalk = \innerList ->
        innerList
        |> List.walkTry
            (innerList |> List.len |> List.withCapacity)
            (\state, elem ->
                when elem |> Str.toU32 is
                    Ok num -> Ok (state |> List.append num)
                    Err InvalidNumStr -> Err (UnableToParseAsU32 elem))

    list |> List.mapTry innerWalk

expect (splitByEmptyString []) == []
expect (splitByEmptyString ["", "", ""]) == []
expect (splitByEmptyString ["a"]) == [["a"]]
expect (splitByEmptyString ["a", ""]) == [["a"]]
expect (splitByEmptyString ["a", "", "b"]) == [["a"], ["b"]]
expect (splitByEmptyString ["a", "", "", "b"]) == [["a"], ["b"]]
expect (splitByEmptyString ["", "a", "", "", "b"]) == [["a"], ["b"]]
expect (splitByEmptyString ["a", "", "b", ""]) == [["a"], ["b"]]
expect (splitByEmptyString ["a", "", "b", "c", ""]) == [["a"], ["b", "c"]]
expect (splitByEmptyString ["a", "b", "", "c", ""]) == [["a", "b"], ["c"]]

expect (tryParseToU32 []) == Ok []
expect (tryParseToU32 [["1"]]) == Ok [[1]]
expect (tryParseToU32 [["1", "2"]]) == Ok [[1, 2]]
expect (tryParseToU32 [["1", "2"], ["3"]]) == Ok [[1, 2], [3]]
expect (tryParseToU32 [["1", "2"], [""]]) == Err (UnableToParseAsU32 "")
expect (tryParseToU32 [["1", "2"], ["-1"]]) == Err (UnableToParseAsU32 "-1")
expect (tryParseToU32 [["1", "2"], ["NaN"]]) == Err (UnableToParseAsU32 "NaN")

expect (tryGetCaloriesCarriedByElfWithMost example) == Ok 24000
expect (tryGetCaloriesCarriedByElfWithMost input) == Ok 68787
