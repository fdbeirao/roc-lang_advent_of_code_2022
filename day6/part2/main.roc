app "app"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.2/tE4xS_zLdmmxmHwHih9kHWQ7fsXtJr7W7h3425-eZFk.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        "../input.txt" as input : Str,
    ]
    provides [main] to pf

allDifferent : List Str -> Bool
allDifferent = \graphemes ->
    (graphemes |> Set.fromList |> Set.len)
    ==
    (graphemes |> List.len)

clamppedAppend : a, List a, Nat -> List a
clamppedAppend = \elem, list, clampFactor ->
    originalListLen = list |> List.len
    if originalListLen < clampFactor then
        list
        |> List.append elem
    else
        list
        |> List.append elem
        |> List.split (originalListLen + 1 - clampFactor)
        |> .others

trySolvePuzzle : Str -> Result Nat [NoStartOfMessageFound]
trySolvePuzzle = \rawInput ->
    nonRepetingGraphemesRequired = 14

    inputGraphemes = rawInput |> Str.graphemes

    if inputGraphemes |> List.len < nonRepetingGraphemesRequired then
        Err NoStartOfMessageFound
    else
        walkFunc = \{ currentSegment, currentIndex, found: _ }, currentGrapheme ->
            newSegment = currentGrapheme |> clamppedAppend currentSegment nonRepetingGraphemesRequired

            newIndex = currentIndex + 1

            keepSearching = Continue { currentSegment: newSegment, currentIndex: newIndex, found: Bool.false }

            break = Break { currentSegment: newSegment, currentIndex: newIndex, found: Bool.true }

            if newIndex >= nonRepetingGraphemesRequired && newSegment |> allDifferent then
                break
            else
                keepSearching

        inputGraphemes
        |> List.walkUntil
            { currentSegment: [], currentIndex: 0, found: Bool.false }
            walkFunc
        |> \{ currentSegment: _, currentIndex, found } ->
            if found then
                Ok currentIndex
            else
                Err NoStartOfMessageFound

main : Task {} []
main =
    prettyPrint = \label, rawInput ->
        when (rawInput |> trySolvePuzzle) is
            Ok result ->
                resultStr = result |> Num.toStr
                "\(label): result is \(resultStr)"

            Err NoStartOfMessageFound ->
                "\(label): No start of message found"

    (prettyPrint "input" input) |> Stdout.line

expect (allDifferent ("" |> Str.graphemes)) == Bool.true
expect (allDifferent ("a" |> Str.graphemes)) == Bool.true
expect (allDifferent ("ab" |> Str.graphemes)) == Bool.true
expect (allDifferent ("aA" |> Str.graphemes)) == Bool.true
expect (allDifferent ("aa" |> Str.graphemes)) == Bool.false

expect ("a" |> clamppedAppend [] 4) == ["a"]
expect ("b" |> clamppedAppend ["a"] 4) == ["a", "b"]
expect ("d" |> clamppedAppend ["a", "b", "c"] 4) == ["a", "b", "c", "d"]
expect ("e" |> clamppedAppend ["a", "b", "c", "d"] 4) == ["b", "c", "d", "e"]
expect ("f" |> clamppedAppend ["a", "b", "c", "d", "e"] 4) == ["c", "d", "e", "f"]

expect (trySolvePuzzle "") == Err NoStartOfMessageFound
expect (trySolvePuzzle "a") == Err NoStartOfMessageFound
expect (trySolvePuzzle "ab") == Err NoStartOfMessageFound
expect (trySolvePuzzle "abc") == Err NoStartOfMessageFound
expect (trySolvePuzzle "abcdefghijklm") == Err NoStartOfMessageFound
expect (trySolvePuzzle "abcdefghijklma") == Err NoStartOfMessageFound
expect (trySolvePuzzle "abcdefghijklmn") == Ok 14
expect (trySolvePuzzle "mjqjpqmgbljsphdztnvjfqwrcgsmlb") == Ok 19
expect (trySolvePuzzle "bvwbjplbgvbhsrlpgdmjqwftvncz") == Ok 23
expect (trySolvePuzzle "nppdvjthqldpwncqszvftbrmjlhg") == Ok 23
expect (trySolvePuzzle "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") == Ok 29
expect (trySolvePuzzle "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") == Ok 26
expect (trySolvePuzzle input) == Ok 3380