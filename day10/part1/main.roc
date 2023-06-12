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
}

init : Model
init = {
    x: 1,
    cycle: 0,
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

# Find the signal strength during the 20th, 60th, 100th, 140th, 180th, and 220th
# cycles. What is the sum of these six signal strengths?
trySolvePuzzle : Str -> Result I32 [UnableToParseLine Str]
trySolvePuzzle = \rawInput ->
    relevantCycles = [20, 60, 100, 140, 180, 220]

    applyOperationAndAddResults : (Model, I32), Operation -> (Model, I32)
    applyOperationAndAddResults = \(model, total), operation ->
        updatedModel = model |> applyOperation operation

        if relevantCycles |> List.contains updatedModel.cycle then
            (updatedModel, updatedModel.cycle * model.x + total)
        else
            (updatedModel, total)

    rawInput
    |> tryParseInput
    |> Result.map
        (\instructions ->
            instructions
            |> convertToOperations
            |> List.walk (init, 0) applyOperationAndAddResults
        )
    |> Result.map (\(_, total) -> total)

main : Task {} []
main =
    prettyPrint = \label, rawInput ->
        when (rawInput |> trySolvePuzzle) is
            Ok result ->
                resultStr = result |> Num.toStr
                "\(label): result is \(resultStr)"

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

## Tests for trySolvePuzzle
expect
    expected = Ok 13140
    actually = example |> trySolvePuzzle
    expected == actually

expect
    expected = Ok 15220
    actually = puzzle |> trySolvePuzzle
    expected == actually
