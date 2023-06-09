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

Move : [Rock, Paper, Scissors]

RoundOutcome : [Lose, Draw, Win]

RoundMoves : { opponent : Move, me : Move }

moveScore : Move -> U8
moveScore = \move ->
    when move is
        Rock -> 1
        Paper -> 2
        Scissors -> 3

roundOutcomeScore : RoundOutcome -> U8
roundOutcomeScore = \roundOutcome ->
    when roundOutcome is
        Lose -> 0
        Draw -> 3
        Win -> 6

roundScore : Move, RoundOutcome -> U8
roundScore = \move, roundOutcome ->
    (move |> moveScore) + (roundOutcome |> roundOutcomeScore)

tryParseOpponentMove : Str -> Result Move [InvalidMoveInput Str]
tryParseOpponentMove = \opponentMove ->
    when opponentMove is
        "A" -> Ok Rock
        "B" -> Ok Paper
        "C" -> Ok Scissors
        _ -> Err (InvalidMoveInput opponentMove)

tryParseMyMove : Str -> Result Move [InvalidMoveInput Str]
tryParseMyMove = \myMove ->
    when myMove is
        "X" -> Ok Rock
        "Y" -> Ok Paper
        "Z" -> Ok Scissors
        _ -> Err (InvalidMoveInput myMove)

determineRoundOutcomeForMe : RoundMoves -> RoundOutcome
determineRoundOutcomeForMe = \{ opponent, me } ->
    when (opponent, me) is
        (Rock, Paper) -> Win
        (Paper, Scissors) -> Win
        (Scissors, Rock) -> Win
        (Rock, Scissors) -> Lose
        (Paper, Rock) -> Lose
        (Scissors, Paper) -> Lose
        _ -> Draw

determineRoundScoreForMe : RoundMoves -> U8
determineRoundScoreForMe = \roundMoves ->
    outcomeForMe = roundMoves |> determineRoundOutcomeForMe
    roundScore roundMoves.me outcomeForMe

tryParseLineAsRound : Str -> Result RoundMoves [InvalidRoundLineInput Str]
tryParseLineAsRound = \roundLine ->
    lineMoves = roundLine |> Str.split " "

    maybeOpponentMove = lineMoves |> List.first |> Result.try tryParseOpponentMove

    maybeMyMove = lineMoves |> List.last |> Result.try tryParseMyMove

    when (maybeOpponentMove, maybeMyMove) is
        (Ok opponentMove, Ok myMove) -> Ok { opponent: opponentMove, me: myMove }
        _ -> Err (InvalidRoundLineInput roundLine)

tryGetGameScore : Str -> Result U32 [InvalidRoundLineInput Str]
tryGetGameScore = \rawInput ->
    allRounds = rawInput |> Str.split "\n" |> List.dropIf Str.isEmpty

    allRounds
    |> List.mapTry tryParseLineAsRound
    |> Result.map (\l -> l |> List.map determineRoundScoreForMe |> List.map Num.toU32 |> List.sum)

main : Task {} []
main =
    prettyRes = \label, gameStr ->
        res = gameStr |> tryGetGameScore
        when res is
            Ok gameScore ->
                gameScoreStr = gameScore |> Num.toStr
                "\(label): score = \(gameScoreStr)"

            Err (InvalidRoundLineInput line) ->
                "\(label): unable to parse [\(line)] as valid input"

    _ <- (prettyRes "example" example) |> Stdout.line |> Task.await

    (prettyRes "puzzle" puzzle) |> Stdout.line

expect (tryParseOpponentMove "A") == Ok Rock
expect (tryParseOpponentMove "B") == Ok Paper
expect (tryParseOpponentMove "C") == Ok Scissors

expect (tryParseMyMove "X") == Ok Rock
expect (tryParseMyMove "Y") == Ok Paper
expect (tryParseMyMove "Z") == Ok Scissors

expect (roundScore Paper Win) == 8
expect (roundScore Rock Lose) == 1
expect (roundScore Scissors Draw) == 6

expect (determineRoundOutcomeForMe { opponent: Rock, me: Rock }) == Draw
expect (determineRoundOutcomeForMe { opponent: Rock, me: Paper }) == Win
expect (determineRoundOutcomeForMe { opponent: Rock, me: Scissors }) == Lose
expect (determineRoundOutcomeForMe { opponent: Paper, me: Rock }) == Lose
expect (determineRoundOutcomeForMe { opponent: Paper, me: Paper }) == Draw
expect (determineRoundOutcomeForMe { opponent: Paper, me: Scissors }) == Win
expect (determineRoundOutcomeForMe { opponent: Scissors, me: Rock }) == Win
expect (determineRoundOutcomeForMe { opponent: Scissors, me: Paper }) == Lose
expect (determineRoundOutcomeForMe { opponent: Scissors, me: Scissors }) == Draw

expect (tryParseLineAsRound "A Y") == Ok { opponent: Rock, me: Paper }
expect (tryParseLineAsRound "B X") == Ok { opponent: Paper, me: Rock }
expect (tryParseLineAsRound "C Z") == Ok { opponent: Scissors, me: Scissors }

expect (tryGetGameScore "A Y") == Ok 8
expect (tryGetGameScore "A Y\nB X") == Ok 9
expect (tryGetGameScore "A Y\nB X\nC Z") == Ok 15
expect (tryGetGameScore example) == Ok 15
expect (tryGetGameScore puzzle) == Ok 13682
