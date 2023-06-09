app "app"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.2/tE4xS_zLdmmxmHwHih9kHWQ7fsXtJr7W7h3425-eZFk.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        "../example.txt" as example : Str,
        "../input.txt" as input : Str,
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

# A tree is visible if all of the other trees between it and an edge of the grid
# are *shorter* than it. Only consider trees in the same row or column; that is,
# only look up, down, left, or right from any given tree.
isTreeVisible : Grid U8, Cell U8 -> Bool
isTreeVisible = \grid, (rowIndex, colIndex, treeHeight) ->
    rowIndexNat = rowIndex |> Num.toNat
    colIndexNat = colIndex |> Num.toNat

    allTreesShorter = \trees ->
        isShorter = \(_, _, height) -> height < treeHeight

        (trees.before |> List.all isShorter)
        || (trees.others |> List.dropFirst |> List.all isShorter)

    treesInSameRow = grid |> getRow rowIndex |> List.split colIndexNat

    if treesInSameRow |> allTreesShorter then
        # Small optimization, because computing the trees in column is a bit more expensive
        Bool.true
    else
        treesInSameColumn = grid |> getColumn colIndex |> List.split rowIndexNat

        treesInSameColumn |> allTreesShorter

# Consider your map; how many trees are visible from outside the grid?
trySolvePuzzle : Str -> Result Nat [UnableToParseAsU8 Str]
trySolvePuzzle = \rawInput ->
    maybeGrid = rawInput |> tryParseIntoGrid
    maybeGrid
    |> Result.map
        (\grid -> grid
            |> getAllCells
            |> List.countIf (\tree -> grid |> isTreeVisible tree))

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

    (prettyPrint "input" input) |> Stdout.line

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

## Tests for isTreeVisible

# The top-left 5 is visible from the left and top.
expect
    expected = Ok Bool.true
    actually = exampleGrid |> Result.try (\grid -> grid |> tryGetCell 1 1 |> Result.map (\tree -> grid |> isTreeVisible tree))
    expected == actually

# The top-middle 5 is visible from the top and right.
expect
    expected = Ok Bool.true
    actually = exampleGrid |> Result.try (\grid -> grid |> tryGetCell 1 2 |> Result.map (\tree -> grid |> isTreeVisible tree))
    expected == actually

# The top-right 1 is not visible from any direction.
expect
    expected = Ok Bool.false
    actually = exampleGrid |> Result.try (\grid -> grid |> tryGetCell 1 3 |> Result.map (\tree -> grid |> isTreeVisible tree))
    expected == actually

# The left-middle 5 is visible, but only from the right.
expect
    expected = Ok Bool.true
    actually = exampleGrid |> Result.try (\grid -> grid |> tryGetCell 2 1 |> Result.map (\tree -> grid |> isTreeVisible tree))
    expected == actually

# The center 3 is not visible from any direction
expect
    expected = Ok Bool.false
    actually = exampleGrid |> Result.try (\grid -> grid |> tryGetCell 2 2 |> Result.map (\tree -> grid |> isTreeVisible tree))
    expected == actually

# The right-middle 3 is visible from the right
expect
    expected = Ok Bool.true
    actually = exampleGrid |> Result.try (\grid -> grid |> tryGetCell 2 3 |> Result.map (\tree -> grid |> isTreeVisible tree))
    expected == actually

## Tests for trySolvePuzzle

# With 16 trees visible on the edge and another 5 visible in the interior, a total of 21 trees are visible in this arrangement.

expect
    expected = Ok 21
    actually = example |> trySolvePuzzle
    expected == actually

expect
    expected = Ok 1538
    actually = input |> trySolvePuzzle
    expected == actually
