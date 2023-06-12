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

Operation : [NoOp, PrepareAddX, PerformAddX I32]

Instruction : [NoOp, AddX I32]

Model : {
    x : I32,
    cycle : I32,
    rows : List Str,
}

init : Model
init = {
    x: 1,
    cycle: 0,
    rows: {} |> List.repeat 6 |> List.map (\{} -> Str.withCapacity 40),
}

applyOperation : Model, Operation -> Model
applyOperation = \model, operation ->
    updatedModel = { model & cycle: model.cycle + 1 }

    when operation is
        NoOp | PrepareAddX -> updatedModel
        PerformAddX howMuch -> { updatedModel & x: updatedModel.x + howMuch }

convertToOperations : List Instruction -> List Operation
convertToOperations = \instructions ->
    walkFunc = \operations, instruction ->
        when instruction is
            NoOp -> operations |> List.append NoOp
            AddX howMuch -> operations |> List.concat [PrepareAddX, PerformAddX howMuch]
    instructions |> List.walk [] walkFunc

# The sprite is 3 pixels wide, and the X register sets the horizontal position
# of the middle of that sprite.
# -1 = #.......................................
#  0 = ##......................................
#  1 = ###.....................................
#  2 = .###....................................
# 38 = ....................................###.
# 39 = .....................................###
# 40 = ......................................##
# 41 = .......................................#
getSprite : I32 -> Str
getSprite = \spriteCenter ->
    dotsBefore = max 0 (spriteCenter - 1) |> Num.toNat
    spriteChars =
        when spriteCenter is
            -1 | 41 -> 1
            0 | 40 -> 2
            _ -> 3
    dotsAfter = max 0 (38 - spriteCenter) |> Num.toNat

    Str.withCapacity 40
    |> Str.concat ("." |> Str.repeat dotsBefore)
    |> Str.concat ("#" |> Str.repeat spriteChars)
    |> Str.concat ("." |> Str.repeat dotsAfter)

max : I32, I32 -> I32
max = \a, b ->
    if a > b then
        a
    else
        b

tryParseInstruction : Str -> Result Instruction [UnableToParseLine Str]
tryParseInstruction = \rawLine ->
    when rawLine |> Str.split " " is
        ["noop"] -> Ok NoOp
        ["addx", rawHowMuch] ->
            when Str.toI32 rawHowMuch is
                Ok howMuch -> Ok (AddX howMuch)
                Err _ -> Err (UnableToParseLine rawLine)

        _ -> Err (UnableToParseLine rawLine)

tryParseInput : Str -> Result (List Instruction) [UnableToParseLine Str]
tryParseInput = \rawInput ->
    rawInput
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.mapTry tryParseInstruction

# Render the image given by your program. What eight capital letters appear on
# your CRT?
trySolvePuzzle : Str -> Result (List Str) [UnableToParseLine Str]
trySolvePuzzle = \rawInput ->

    walkFunc : Model, Operation -> Model
    walkFunc = \oldModel, operation ->
        model = oldModel |> applyOperation operation

        sprite = oldModel.x |> getSprite
        colIndex = (model.cycle - 1) % 40 |> Num.toNat
        graphemeToAdd = sprite |> Str.graphemes |> List.get colIndex |> Result.withDefault "?"

        rowIndex = (model.cycle - 1) // 40 |> Num.toNat
        updatedRows = model.rows |> List.update rowIndex (\line -> line |> Str.concat graphemeToAdd)

        { model & rows: updatedRows }

    rawInput
    |> tryParseInput
    |> Result.map
        (\instructions ->
            instructions
            |> convertToOperations
            |> List.walk init walkFunc
        )
    |> Result.map .rows

main : Task {} []
main =
    prettyPrint = \label, rawInput ->
        when (rawInput |> trySolvePuzzle) is
            Ok result ->
                lines = result |> Str.joinWith "\n"
                "\(label):\n\(lines)\n"

            Err (UnableToParseLine reason) ->
                "\(label): Unable to parse line [\(reason)]"

    _ <- (prettyPrint "example" example) |> Stdout.line |> Task.await

    (prettyPrint "puzzle" puzzle) |> Stdout.line

## ── TESTS ────────────────────────────────────────────────────────────────────

## Tests for tryParseInstruction
expect
    expected = Ok NoOp
    actually = "noop" |> tryParseInstruction
    expected == actually

expect
    expected = Ok (AddX 5)
    actually = "addx 5" |> tryParseInstruction
    expected == actually

expect
    expected = Ok (AddX -5)
    actually = "addx -5" |> tryParseInstruction
    expected == actually

expect
    expected = Err (UnableToParseLine "add 5")
    actually = "add 5" |> tryParseInstruction
    expected == actually

## Tests for tryParseInput
expect
    expected = Ok 146
    actually = example |> tryParseInput |> Result.map List.len
    expected == actually

## Tests for convertToOperations
expect
    expected = [NoOp, NoOp, PrepareAddX, PerformAddX 5, PrepareAddX, PerformAddX 3, NoOp]
    actually = [NoOp, NoOp, AddX 5, AddX 3, NoOp] |> convertToOperations
    expected == actually

## Tests for getSprite
expect
    expected = [
        "#.......................................",
        "##......................................",
        "###.....................................",
        ".###....................................",
        "..###...................................",
        "...###..................................",
        "....###.................................",
        ".....###................................",
        "......###...............................",
        ".......###..............................",
        "........###.............................",
        ".........###............................",
        "..........###...........................",
        "...........###..........................",
        "............###.........................",
        ".............###........................",
        "..............###.......................",
        "...............###......................",
        "................###.....................",
        ".................###....................",
        "..................###...................",
        "...................###..................",
        "....................###.................",
        ".....................###................",
        "......................###...............",
        ".......................###..............",
        "........................###.............",
        ".........................###............",
        "..........................###...........",
        "...........................###..........",
        "............................###.........",
        ".............................###........",
        "..............................###.......",
        "...............................###......",
        "................................###.....",
        ".................................###....",
        "..................................###...",
        "...................................###..",
        "....................................###.",
        ".....................................###",
        "......................................##",
        ".......................................#",
    ]
    actually = expected |> List.mapWithIndex (\_, index -> ((index |> Num.toI32) - 1) |> getSprite)
    expected == actually

## Tests for trySolvePuzzle
expect
    expected = Ok [
        "##..##..##..##..##..##..##..##..##..##..",
        "###...###...###...###...###...###...###.",
        "####....####....####....####....####....",
        "#####.....#####.....#####.....#####.....",
        "######......######......######......####",
        "#######.......#######.......#######.....",
    ]
    actually = example |> trySolvePuzzle
    expected == actually

expect
    expected = Ok [
        "###..####.####.####.#..#.###..####..##..",
        "#..#.#.......#.#....#.#..#..#.#....#..#.",
        "#..#.###....#..###..##...###..###..#..#.",
        "###..#.....#...#....#.#..#..#.#....####.",
        "#.#..#....#....#....#.#..#..#.#....#..#.",
        "#..#.#....####.####.#..#.###..#....#..#.",
    ]
    actually = puzzle |> trySolvePuzzle
    expected == actually
