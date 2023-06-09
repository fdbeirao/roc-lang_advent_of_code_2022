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

Model : {
    head : Coordinates,
    tail : Coordinates,
    tailVisited : Set Coordinates,
}

Coordinates : (I32, I32)

Direction : [Up, Right, Down, Left]

MoveCommand : { direction : Direction, howMany : Nat }

init : Model
init = {
    head: origin,
    tail: origin,
    tailVisited: Set.empty {} |> Set.insert origin,
}

origin : Coordinates
origin = (0, 0)

applyMove : Model, Direction -> Model
applyMove = \modelBeforeMove, direction ->
    add : Coordinates, Coordinates -> Coordinates
    add = \(x1, y1), (x2, y2) ->
        (x1 + x2, y1 + y2)

    moveHead : Model -> Model
    moveHead = \model ->
        delta =
            when direction is
                Up -> (0, -1)
                Right -> (1, 0)
                Down -> (0, 1)
                Left -> (-1, 0)

        { model & head: model.head |> add delta }

    moveTailIfNecessary : Model -> Model
    moveTailIfNecessary = \model ->
        if model.tail |> touching model.head then
            model
        else
            { model &
                tail: modelBeforeMove.head,
                tailVisited: model.tailVisited |> Set.insert modelBeforeMove.head,
            }

    modelBeforeMove
    |> moveHead
    |> moveTailIfNecessary

touching : Coordinates, Coordinates -> Bool
touching = \(hx, hy), (tx, ty) ->
    when (Num.abs (hx - tx), Num.abs (hy - ty)) is
        (0, 0)
        | (1, 0)
        | (0, 1)
        | (1, 1) -> Bool.true

        _ -> Bool.false

tryParseInput : Str -> Result (List MoveCommand) [UnableToParseLine Str]
tryParseInput = \rawInput ->
    parseLine = \rawLine ->
        unableToParse = Err (UnableToParseLine rawLine)

        when rawLine |> Str.split " " is
            [rawDirection, rawHowMany] ->
                when (rawDirection, rawHowMany |> Str.toNat) is
                    ("U", Ok howMany) -> Ok { direction: Up, howMany }
                    ("R", Ok howMany) -> Ok { direction: Right, howMany }
                    ("D", Ok howMany) -> Ok { direction: Down, howMany }
                    ("L", Ok howMany) -> Ok { direction: Left, howMany }
                    _ -> unableToParse

            _ -> unableToParse

    rawInput
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.mapTry parseLine

# Simulate your complete hypothetical series of motions. How many positions does
# the tail of the rope visit at least once?
trySolvePuzzle : Str -> Result Nat [UnableToParseLine Str]
trySolvePuzzle = \rawInput ->
    expandMoves = \moves, { direction, howMany } ->
        moves |> List.concat (direction |> List.repeat howMany)

    rawInput
    |> tryParseInput
    |> Result.map
        (\moves ->
            moves
            |> List.walk [] expandMoves
            |> List.walk init applyMove
            |> .tailVisited
            |> Set.len
        )

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

## Tests for touching

expect
    expected = [
        ((0, 0), (0, 0), Bool.true),
        ((1, 0), (0, 0), Bool.true),
        ((0, 0), (1, 0), Bool.true),
        ((1, 0), (0, 1), Bool.true),
        ((0, 1), (1, 0), Bool.true),
        ((-1, 0), (1, 0), Bool.false),
        ((1, 0), (-1, 0), Bool.false),
        ((0, -1), (0, 1), Bool.false),
        ((0, 1), (0, -1), Bool.false),
    ]
    actually = expected |> List.map (\(a, b, _) -> (a, b, touching a b))
    expected == actually

## Tests for applyMove

# ↖ ↑ ↗
# ← · →
# ↙ ↓ ↘

modelWith : { head : Coordinates, tail : Coordinates, tailVisited : List Coordinates } -> Model
modelWith = \{ head, tail, tailVisited } -> {
    head,
    tail,
    tailVisited: tailVisited |> Set.fromList,
}

# H: ↑ | T: ·
expect
    expected = modelWith { head: (0, -1), tail: (0, 0), tailVisited: [(0, 0)] }
    actually = init |> applyMove Up
    expected == actually

# H: → | T: ·
expect
    expected = modelWith { head: (1, 0), tail: (0, 0), tailVisited: [(0, 0)] }
    actually = init |> applyMove Right
    expected == actually

# H: ↓ | T: ·
expect
    expected = modelWith { head: (0, 1), tail: (0, 0), tailVisited: [(0, 0)] }
    actually = init |> applyMove Down
    expected == actually

# H: ← | T: ·
expect
    expected = modelWith { head: (-1, 0), tail: (0, 0), tailVisited: [(0, 0)] }
    actually = init |> applyMove Left
    expected == actually

# H: ↑ ↑ | T: ↑
expect
    expected = modelWith { head: (0, -2), tail: (0, -1), tailVisited: [(0, 0), (0, -1)] }
    actually = init |> applyMove Up |> applyMove Up
    expected == actually

# H: → → | T: →
expect
    expected = modelWith { head: (2, 0), tail: (1, 0), tailVisited: [(0, 0), (1, 0)] }
    actually = init |> applyMove Right |> applyMove Right
    expected == actually

# H: ↓ ↓ | T: ↓
expect
    expected = modelWith { head: (0, 2), tail: (0, 1), tailVisited: [(0, 0), (0, 1)] }
    actually = init |> applyMove Down |> applyMove Down
    expected == actually

# H: ← ← | T: ←
expect
    expected = modelWith { head: (-2, 0), tail: (-1, 0), tailVisited: [(0, 0), (-1, 0)] }
    actually = init |> applyMove Left |> applyMove Left
    expected == actually

## Tests for tryParseInput

expect
    expected = Ok [{ direction: Right, howMany: 4 }, { direction: Up, howMany: 3 }, { direction: Left, howMany: 2 }, { direction: Down, howMany: 1 }]
    actually = "R 4\nU 3\nL 2\nD 1\n" |> tryParseInput
    expected == actually

expect
    expected = Ok 8
    actually = example |> tryParseInput |> Result.map List.len
    expected == actually

## Tests for trySolvePuzzle

# So, there are 13 positions the tail visited at least once.
expect
    expected = Ok 13
    actually = example |> trySolvePuzzle
    expected == actually

expect
    expected = Ok 6486
    actually = puzzle |> trySolvePuzzle
    expected == actually
