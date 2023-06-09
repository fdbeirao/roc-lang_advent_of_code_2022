app "app"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.2/tE4xS_zLdmmxmHwHih9kHWQ7fsXtJr7W7h3425-eZFk.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        "../example.txt" as example : Str,
        "../puzzle.txt" as puzzle : Str,
    ]
    provides [main] to pf

# [ A B C ]
# [ D E F ]
# [ G H I ]
# A(0,0) B(1,0) C(2,0)
# D(1,0) E(1,1) F(1,2)
# G(2,0) H(2,1) I(2,2)

CoordinateType : U8
Cell a : (CoordinateType, CoordinateType, a)
Grid a : List (List (Cell a))

tryParseIntoGrid : Str -> Result (Grid U8) [UnableToParseAsU8 Str]
tryParseIntoGrid = \rawInput ->
    tryParseRow = \rows, rawLine ->
        # ⚠ Here be dragons, if the total rows is larger than 255 (Num.maxU8)
        rowIndex = rows |> List.len |> Num.toU8

        tryParseCells = \cells, grapheme ->
            # ⚠ Here be dragons, if the row is larger than 255 (Num.maxU8)
            cellIndex = cells |> List.len |> Num.toU8

            when grapheme |> Str.toU8 is
                Ok value ->
                    cell = (rowIndex, cellIndex, value)
                    Ok (cells |> List.append cell)

                Err InvalidNumStr ->
                    Err (UnableToParseAsU8 grapheme)

        rawLine
        |> Str.graphemes
        |> List.walkTry [] tryParseCells
        |> Result.map (\cellsInRow -> rows |> List.append cellsInRow)

    rawInput
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.walkTry [] tryParseRow

getAllCells : Grid a -> List (Cell a)
getAllCells = \grid ->
    grid |> List.joinMap (\row -> row)

getRow : Grid a, CoordinateType -> List (Cell a)
getRow = \grid, rowIndex ->
    rowIndexNat = rowIndex |> Num.toNat

    grid
    |> List.get rowIndexNat
    |> Result.withDefault []

getColumn : Grid a, CoordinateType -> List (Cell a)
getColumn = \grid, colIndex ->
    colIndexNat = colIndex |> Num.toNat

    grid
    |> List.mapTry (\col -> col |> List.get colIndexNat)
    |> Result.withDefault []

tryGetCell : Grid a, CoordinateType, CoordinateType -> Result (Cell a) [OutOfBounds]
tryGetCell = \grid, rowIndex, colIndex ->
    rowIndexNat = rowIndex |> Num.toNat
    colIndexNat = colIndex |> Num.toNat

    grid
    |> List.get rowIndexNat
    |> Result.try (\row -> row |> List.get colIndexNat)

# A tree's scenic score is found by multiplying together its viewing distance in
# each of the four directions.
getTreeScenicScore : Grid U8, Cell U8 -> Nat
getTreeScenicScore = \grid, (rowIndex, colIndex, treeHeight) ->
    rowIndexNat = rowIndex |> Num.toNat
    colIndexNat = colIndex |> Num.toNat

    treesInSameRow = grid |> getRow rowIndex |> List.split colIndexNat
    treesInSameCol = grid |> getColumn colIndex |> List.split rowIndexNat

    walkFunc = \distanceSoFar, (_, _, otherTreeheight) ->
        if otherTreeheight >= treeHeight then
            Break (distanceSoFar + 1)
        else
            Continue (distanceSoFar + 1)

    viewingDistance = \direction ->
        when direction is
            Up -> treesInSameCol.before |> List.walkBackwardsUntil 0 walkFunc
            Left -> treesInSameRow.before |> List.walkBackwardsUntil 0 walkFunc
            Right -> treesInSameRow.others |> List.dropFirst |> List.walkUntil 0 walkFunc
            Down -> treesInSameCol.others |> List.dropFirst |> List.walkUntil 0 walkFunc

    viewingDistance Up
    * viewingDistance Left
    * viewingDistance Right
    * viewingDistance Down

# Consider each tree on your map. What is the highest scenic score possible for
# any tree?
trySolvePuzzle : Str -> Result Nat [UnableToParseAsU8 Str]
trySolvePuzzle = \rawInput ->
    maybeGrid = rawInput |> tryParseIntoGrid
    maybeGrid
    |> Result.map
        (\grid -> grid
            |> getAllCells
            |> List.map (\tree -> grid |> getTreeScenicScore tree)
            |> List.max
            |> Result.withDefault 0)

main : Task {} []
main =
    prettyPrint = \label, rawInput ->
        when (rawInput |> trySolvePuzzle) is
            Ok result ->
                resultStr = result |> Num.toStr
                "\(label): result is \(resultStr)"

            Err (UnableToParseAsU8 reason) ->
                "\(label): Unable to parse [\(reason)] as U8"

    _ <- (prettyPrint "example" example) |> Stdout.line |> Task.await

    (prettyPrint "puzzle" puzzle) |> Stdout.line

## -----------------------------------------------------------------------------

## Tests for tryParseIntoGrid

small1234 = [[(0, 0, 1), (0, 1, 2)], [(1, 0, 3), (1, 1, 4)]]
exampleGrid = tryParseIntoGrid example

expect
    expected = Ok ([[(0, 0, 1)]])
    actually = tryParseIntoGrid "1"
    expected == actually

expect
    expected = Ok small1234
    actually = tryParseIntoGrid "12\n34"
    expected == actually

## Tests for getAllCells

expect
    expected = [(0, 0, 1), (0, 1, 2), (1, 0, 3), (1, 1, 4)]
    actually = small1234 |> getAllCells
    expected == actually

## Tests for getRow

expect
    expected = []
    actually = small1234 |> getRow 5
    expected == actually

expect
    expected = [(0, 0, 1), (0, 1, 2)]
    actually = small1234 |> getRow 0
    expected == actually

## Tests for getColumn

expect
    expected = []
    actually = small1234 |> getColumn 5
    expected == actually

expect
    expected = [(0, 0, 1), (1, 0, 3)]
    actually = small1234 |> getColumn 0
    expected == actually

## Tests for tryGetCell

expect
    expected = Ok (1, 1, 4)
    actually = small1234 |> tryGetCell 1 1
    expected == actually

expect
    expected = Err OutOfBounds
    actually = small1234 |> tryGetCell 5 1
    expected == actually

expect
    expected = Err OutOfBounds
    actually = small1234 |> tryGetCell 1 5
    expected == actually

## Tests for getTreeScenicScore

# Consider the middle 5 in the second row.
# For this tree, this is 4 (found by multiplying 1 * 1 * 2 * 2)
expect
    expected = Ok 4
    actually = exampleGrid |> Result.try (\grid -> grid |> tryGetCell 1 2 |> Result.map (\tree -> grid |> getTreeScenicScore tree))
    expected == actually

# Consider the tree of height 5 in the middle of the fourth row
# This tree's scenic score is 8 (2 * 2 * 1 * 2)
expect
    expected = Ok 8
    actually = exampleGrid |> Result.try (\grid -> grid |> tryGetCell 3 2 |> Result.map (\tree -> grid |> getTreeScenicScore tree))
    expected == actually

## Tests for trySolvePuzzle

# This tree's scenic score is 8 (2 * 2 * 1 * 2); this is the ideal spot for the tree house.

expect
    expected = Ok 8
    actually = example |> trySolvePuzzle
    expected == actually

expect
    expected = Ok 496125
    actually = puzzle |> trySolvePuzzle
    expected == actually
