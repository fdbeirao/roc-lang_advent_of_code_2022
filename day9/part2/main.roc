app "app"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.2/tE4xS_zLdmmxmHwHih9kHWQ7fsXtJr7W7h3425-eZFk.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        "../example2.txt" as example : Str,
        "../puzzle.txt" as puzzle : Str,
    ]
    provides [main] to pf

Model : {
    # [ H, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
    rope : List Coordinates,
    tailVisited : Set Coordinates,
}

Coordinates : (I32, I32)

Direction : [Up, Right, Down, Left]

MoveCommand : { direction : Direction, howMany : Nat }

init : Model
init = {
    rope: origin |> List.repeat 10,
    tailVisited: Set.empty {} |> Set.insert origin,
}

origin : Coordinates
origin = (0, 0)

applyMove : Model, Direction -> Model
applyMove = \modelBeforeMove, direction ->
    moveHead : Model -> Model
    moveHead = \model ->
        delta =
            when direction is
                Up -> (0, -1)
                Down -> (0, 1)
                Right -> (1, 0)
                Left -> (-1, 0)

        { model & rope: model.rope |> List.update 0 (\head -> head |> addCoordinates delta) }

    pullTailElements : Model -> Model
    pullTailElements = \model ->
        walkFunc = \(rope, index), current ->
            if (index == 0) then
                (rope, index + 1)
            else
                when rope |> List.get (index - 1) is
                    Ok previous -> (rope |> List.set index (current |> pullTowards previous), index + 1)
                    Err OutOfBounds -> (rope, index + 1)

        (updatedRope, _) = model.rope |> List.walk (model.rope, 0) walkFunc
        { model & rope: updatedRope }

    updateTailVisited : Model -> Model
    updateTailVisited = \model ->
        updatedTailVisited = model.tailVisited |> Set.insert (model.rope |> List.last |> Result.withDefault origin)

        { model & tailVisited: updatedTailVisited }

    modelBeforeMove
    |> moveHead
    |> pullTailElements
    |> updateTailVisited

addCoordinates : Coordinates, Coordinates -> Coordinates
addCoordinates = \(x1, y1), (x2, y2) ->
    (x1 + x2, y1 + y2)

# If the head is ever two steps directly up, down, left, or right from the tail,
# the tail must also move one step in that direction so it remains close.
# If the head and tail aren't touching and aren't in the same row or column, the
# tail always moves one step diagonally to keep up
# --
# This function takes two arguments A and B. The usage is a |> pullTowards b
# Therefore b will be the "head", the one that moved away from a (·).
#
# a b c d e
# f 1 2 3 g
# h 4 · 6 i
# j 7 8 9 k
# l m n o p
#
pullTowards : Coordinates, Coordinates -> Coordinates
pullTowards = \(ax, ay), (bx, by) ->
    # When they are touching, there's no need to pullTowards
    (one, two, three, four, noMove, six, seven, eight, nine) = ((-1, -1), (0, -1), (1, -1), (-1, 0), (0, 0), (1, 0), (-1, 1), (0, 1), (1, 1))
    deltaMove =
        when (bx - ax, by - ay) is
            # a
            (-2, -2) -> one
            # b
            (-1, -2) -> one
            # c
            (0, -2) -> two
            # d
            (1, -2) -> three
            # e
            (2, -2) -> three
            # f
            (-2, -1) -> one
            # g
            (2, -1) -> three
            # h
            (-2, 0) -> four
            # i
            (2, 0) -> six
            # j
            (-2, 1) -> seven
            # k
            (2, 1) -> nine
            # l
            (-2, 2) -> seven
            # m
            (-1, 2) -> seven
            # n
            (0, 2) -> eight
            # o
            (1, 2) -> nine
            # p
            (2, 2) -> nine
            _ -> noMove
    (ax, ay) |> addCoordinates deltaMove

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

## Tests for pullTowards

# a b c d e
# f 1 2 3 g
# h 4 · 6 i
# j 7 8 9 k
# l m n o p

# A -> 1
expect
    (one, two, three, four, five, six, seven, eight, nine) = ((-1, -1), (0, -1), (1, -1), (-1, 0), origin, (1, 0), (-1, 1), (0, 1), (1, 1))
    (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = ((-2, -2), (-1, -2), (0, -2), (1, -2), (2, -2), (-2, -1), (2, -1), (-2, 0), (2, 0), (-2, 1), (2, 1), (-2, 2), (-1, 2), (0, 2), (1, 2), (2, 2))
    expected = [
        (a, one),
        (b, one),
        (c, two),
        (d, three),
        (e, three),
        (f, one),
        (g, three),
        (h, four),
        (i, six),
        (j, seven),
        (k, nine),
        (l, seven),
        (m, seven),
        (n, eight),
        (o, nine),
        (p, nine),
        (one, origin),
        (two, origin),
        (three, origin),
        (four, origin),
        (five, origin),
        (six, origin),
        (seven, origin),
        (eight, origin),
        (nine, origin),
    ]
    actually = expected |> List.map (\(headPos, _) -> (headPos, origin |> pullTowards headPos))
    expected == actually

## Tests for applyMove

# ↖ ↑ ↗
# ← · →
# ↙ ↓ ↘

modelWith : { rope : List Coordinates, tailVisited : List Coordinates } -> Model
modelWith = \{ rope, tailVisited } -> {
    rope,
    tailVisited: tailVisited |> Set.fromList,
}

# H: ↑ | T: ·
expect
    expected = modelWith {
        rope: [(0, -1), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)],
        tailVisited: [(0, 0)],
    }
    actually = init |> applyMove Up
    expected == actually

# H: ↑ → ↑ | T: ·
expect
    expected = modelWith {
        rope: [(1, -2), (1, -1), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)],
        tailVisited: [(0, 0)],
    }
    actually = init |> applyMove Up |> applyMove Right |> applyMove Up
    expected == actually

# H: → | T: ·
expect
    expected = modelWith {
        rope: [(1, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)],
        tailVisited: [(0, 0)],
    }
    actually = init |> applyMove Right
    expected == actually

# H: ↓ | T: ·
expect
    expected = modelWith {
        rope: [(0, 1), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)],
        tailVisited: [(0, 0)],
    }
    actually = init |> applyMove Down
    expected == actually

# H: ← | T: ·
expect
    expected = modelWith {
        rope: [(-1, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)],
        tailVisited: [(0, 0)],
    }
    actually = init |> applyMove Left
    expected == actually

# H: ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ | T: ↑
expect
    expected = modelWith {
        rope: [(0, -10), (0, -9), (0, -8), (0, -7), (0, -6), (0, -5), (0, -4), (0, -3), (0, -2), (0, -1)],
        tailVisited: [(0, 0), (0, -1)],
    }
    actually = Up |> List.repeat 10 |> List.walk init (\model, move -> model |> applyMove move)
    expected == actually

# H: → → → → → → → → → → | T: →
expect
    expected = modelWith {
        rope: [(10, 0), (9, 0), (8, 0), (7, 0), (6, 0), (5, 0), (4, 0), (3, 0), (2, 0), (1, 0)],
        tailVisited: [(0, 0), (1, 0)],
    }
    actually = Right |> List.repeat 10 |> List.walk init (\model, move -> model |> applyMove move)
    expected == actually

# H: ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ | T: ↓
expect
    expected = modelWith {
        rope: [(0, 10), (0, 9), (0, 8), (0, 7), (0, 6), (0, 5), (0, 4), (0, 3), (0, 2), (0, 1)],
        tailVisited: [(0, 0), (0, 1)],
    }
    actually = Down |> List.repeat 10 |> List.walk init (\model, move -> model |> applyMove move)
    expected == actually

# H: ← ← ← ← ← ← ← ← ← ← | T: ←
expect
    expected = modelWith {
        rope: [(-10, 0), (-9, 0), (-8, 0), (-7, 0), (-6, 0), (-5, 0), (-4, 0), (-3, 0), (-2, 0), (-1, 0)],
        tailVisited: [(0, 0), (-1, 0)],
    }
    actually = Left |> List.repeat 10 |> List.walk init (\model, move -> model |> applyMove move)
    expected == actually

# H: → → → → → ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ | T: ·
expect
    expected = Ok
        (
            modelWith {
                rope: [(5, -8), (5, -7), (5, -6), (5, -5), (5, -4), (4, -4), (3, -3), (2, -2), (1, -1), (0, 0)],
                tailVisited: [(0, 0)],
            }
        )
    actually =
        "R 5\nU 8\n"
        |> tryParseInput
        |> Result.map
            (\rawMoves -> rawMoves
                |> List.walk
                    []
                    (\moves, { direction, howMany } -> moves
                        |> List.concat (direction |> List.repeat howMany))
                |> List.walk init (\model, move -> model |> applyMove move))
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

# Now, the tail (9) visits 36 positions (including s) at least once.
expect
    expected = Ok 36
    actually = example |> trySolvePuzzle
    expected == actually

expect
    expected = Ok 2678
    actually = puzzle |> trySolvePuzzle
    expected == actually
