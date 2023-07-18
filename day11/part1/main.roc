app "app"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.4.0/DI4lqn7LIZs8ZrCDUgLK-tHHpQmxGF1ZrlevRKq5LXk.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        "../example.txt" as example : Str,
        "../input.txt" as puzzle : Str,
    ]
    provides [main] to pf

WorryLevel : Nat

Operation : [Add Nat, Mul Nat, MulOld]

Test : [DivisibleBy Nat]

Action : [ThrowToMonkey Nat]

Monkey : {
    items : List WorryLevel,
    operation : Operation,
    test : Test,
    ifTrue : Action,
    ifFalse : Action,
}

InspectionsTracker : List Nat

ParseError : [UnableToParse Str Str]

tryParseAsNat : Str -> Result Nat ParseError
tryParseAsNat = \numStr ->
    numStr
    |> Str.toNat
    |> Result.mapErr (\_ -> UnableToParse "tryParseAsNat" numStr)

tryParseStartingItemsLine : Str -> Result (List WorryLevel) ParseError
tryParseStartingItemsLine = \line ->
    when line |> Str.split ": " is
        ["  Starting items", items] -> items |> Str.split ", " |> List.mapTry tryParseAsNat
        _ -> Err (UnableToParse "tryParseStartingItemsLine" line)

tryParseOperationLine : Str -> Result Operation ParseError
tryParseOperationLine = \line ->

    tryParseOperation : Str -> Result Operation ParseError
    tryParseOperation = \operation ->
        when operation |> Str.split " " is
            ["*", "old"] -> Ok MulOld
            ["*", numStr] -> numStr |> tryParseAsNat |> Result.map Mul
            ["+", numStr] -> numStr |> tryParseAsNat |> Result.map Add
            _ -> Err (UnableToParse "tryParseOperationLine/operation" operation)

    when line |> Str.split "= old " is
        ["  Operation: new ", operation] -> operation |> tryParseOperation
        _ -> Err (UnableToParse "tryParseOperationLine" line)

tryParseTestLine : Str -> Result Test ParseError
tryParseTestLine = \line ->

    tryParseTestCondition : Str -> Result Test ParseError
    tryParseTestCondition = \testCondition ->
        when testCondition |> Str.split " " is
            ["divisible", "by", numStr] -> numStr |> tryParseAsNat |> Result.map DivisibleBy
            _ -> Err (UnableToParse "test condition" testCondition)

    when line |> Str.split ": " is
        ["  Test", testCondition] -> testCondition |> tryParseTestCondition
        _ -> Err (UnableToParse "tryParseTestLine" line)

tryParseAction : Str -> Result Action ParseError
tryParseAction = \rawAction ->
    when rawAction |> Str.split " " is
        ["throw", "to", "monkey", numStr] -> numStr |> tryParseAsNat |> Result.map ThrowToMonkey
        _ -> Err (UnableToParse "tryParseAction" rawAction)

tryParseIfLine : Str -> Result Action ParseError
tryParseIfLine = \line ->
    when line |> Str.split ": " is
        ["    If true", rawAction] -> rawAction |> tryParseAction
        ["    If false", rawAction] -> rawAction |> tryParseAction
        _ -> Err (UnableToParse "tryParseIfLine" line)

tryParseMonkey : Str -> Result Monkey ParseError
tryParseMonkey = \rawInput ->
    when rawInput |> Str.split "\n" |> List.dropIf Str.isEmpty is
        [_, startingItemsLine, operationLine, testLine, ifTrueLine, ifFalseLine] ->
            when
                (
                    startingItemsLine |> tryParseStartingItemsLine,
                    operationLine |> tryParseOperationLine,
                    testLine |> tryParseTestLine,
                    ifTrueLine |> tryParseIfLine,
                    ifFalseLine |> tryParseIfLine,
                )
            is
                (Ok items, Ok operation, Ok test, Ok ifTrue, Ok ifFalse) ->
                    Ok {
                        items: items,
                        operation: operation,
                        test: test,
                        ifTrue: ifTrue,
                        ifFalse: ifFalse,
                    }

                _ -> Err (UnableToParse "raw input into lines" rawInput)

        _ -> Err (UnableToParse "raw input" rawInput)

tryParseMonkeys : Str -> Result (List Monkey) ParseError
tryParseMonkeys = \rawInput ->
    rawInput
    |> Str.split "\n\n"
    |> List.mapTry tryParseMonkey

# The monkeys take turns inspecting and throwing items. On a single monkey's
# turn, it inspects and throws all of the items it is holding one at a time and
# in the order listed. Monkey 0 goes first, then monkey 1, and so on until each
# monkey has had one turn. The process of each monkey taking a single turn is
# called a round.
#
# After each monkey inspects an item but before it tests your worry level, your
# relief that the monkey's inspection didn't damage the item causes your worry
# level to be divided by three and rounded down to the nearest integer.
#
# When a monkey throws an item to another monkey, the item goes on the end of
# the recipient monkey's list. A monkey that starts a round with no items could
# end up inspecting and throwing many items by the time its turn comes around.
# If a monkey is holding no items at the start of its turn, its turn ends.

round : (InspectionsTracker, List Monkey) -> (InspectionsTracker, List Monkey)
round = \(roundInitInspectionsTracker, roundInitMonkeys) ->

    turn = \(turnInitInspectionsTracker, turnInitMonkeys), currentMonkeyIndex ->
        when turnInitMonkeys |> List.get currentMonkeyIndex is
            Err OutOfBounds ->
                (turnInitInspectionsTracker, turnInitMonkeys)

            Ok monkey ->
                inspectItem = \monkeys, itemWorryLevel ->
                    # Apply the monkey's operation
                    itemWorryLevelAfterOperation =
                        when monkey.operation is
                            Add howMuch -> itemWorryLevel + howMuch
                            Mul howMuch -> itemWorryLevel * howMuch
                            MulOld -> itemWorryLevel * itemWorryLevel

                    # Relief that the monkey's inspection didn't damage the item
                    newItemWorryLevel = itemWorryLevelAfterOperation // 3

                    # Check the test result
                    testResult =
                        when monkey.test is
                            DivisibleBy howMuch -> newItemWorryLevel % howMuch == 0

                    applyAction = \action ->
                        when action is
                            ThrowToMonkey targetMonkeyIndex ->
                                monkeys
                                |> List.update targetMonkeyIndex (\monkeyToUpdate -> { monkeyToUpdate & items: monkeyToUpdate.items |> List.append newItemWorryLevel })
                                |> List.update currentMonkeyIndex (\monkeyToUpdate -> { monkeyToUpdate & items: monkeyToUpdate.items |> List.dropFirst })

                    # Result of inspecting this item
                    if testResult then
                        applyAction monkey.ifTrue
                    else
                        applyAction monkey.ifFalse

                # The monkey will inspect all items he has
                newInspectionsTracker =
                    turnInitInspectionsTracker
                    |> List.update currentMonkeyIndex (\t -> t + (monkey.items |> List.len))

                # Apply the logic for each item of the current monkey
                monkeysAfterAllItems =
                    monkey.items |> List.walk turnInitMonkeys inspectItem

                # Result of turn
                (newInspectionsTracker, monkeysAfterAllItems)

    # Run each monkey's turn
    List.range { start: At 0, end: Length (roundInitMonkeys |> List.len) }
    |> List.walk (roundInitInspectionsTracker, roundInitMonkeys) turn

runRounds : Nat, List Monkey -> (InspectionsTracker, List Monkey)
runRounds = \howManyRounds, monkeys ->
    inspectionTracker = 0 |> List.repeat (monkeys |> List.len)

    "" |> List.repeat howManyRounds |> List.walk (inspectionTracker, monkeys) (\state, _ -> round state)

# Chasing all of the monkeys at once is impossible; you're going to have to
# focus on the two most active monkeys.
# In this example, the two most active monkeys inspected items 101 and 105 times.
# The level of monkey business in this situation can be found by multiplying
# these together
monkeyBusinessAfterRounds : Nat, List Monkey -> Nat
monkeyBusinessAfterRounds = \howManyRounds, monkeys ->
    (inspectionTracker, _) = runRounds howManyRounds monkeys

    inspectionTracker
    |> List.sortDesc
    |> List.takeFirst 2
    |> List.walk 1 (\s, x -> s * x)


# Figure out which monkeys to chase by counting how many items they inspect over 
# 20 rounds. What is the level of monkey business after 20 rounds of 
# stuff-slinging simian shenanigans?
trySolvePuzzle : Str -> Result Nat ParseError
trySolvePuzzle = \rawInput ->
    rawInput 
    |> tryParseMonkeys
    |> Result.map (\monkeys -> monkeyBusinessAfterRounds 20 monkeys)


main : Task {} U32
main =
    prettyPrint = \label, rawInput ->
        when (rawInput |> trySolvePuzzle) is
            Ok result ->
                resultStr = result |> Num.toStr
                "\(label): result is \(resultStr)"

            Err (UnableToParse location reason) ->
                "\(label): Unable to parse at [\(location)]: [\(reason)]"

    _ <- (prettyPrint "example" example) |> Stdout.line |> Task.await

    (prettyPrint "puzzle" puzzle) |> Stdout.line

## ── TESTS ────────────────────────────────────────────────────────────────────

## Tests for tryParseOperationLine

expect
    expected = Ok (Mul 19)
    actually = "  Operation: new = old * 19" |> tryParseOperationLine
    expected == actually

expect
    expected = Ok (Add 6)
    actually = "  Operation: new = old + 6" |> tryParseOperationLine
    expected == actually

expect
    expected = Ok MulOld
    actually = "  Operation: new = old * old" |> tryParseOperationLine
    expected == actually

## Tests for tryParseMonkey
expect
    monkeyInput =
        """
        Monkey 0:
          Starting items: 78, 98
          Operation: new = old * old
          Test: divisible by 23
            If true: throw to monkey 2
            If false: throw to monkey 4
        """
    expected = Ok {
        items: [78, 98],
        operation: MulOld,
        test: DivisibleBy 23,
        ifTrue: ThrowToMonkey 2,
        ifFalse: ThrowToMonkey 4,
    }
    actually = monkeyInput |> tryParseMonkey
    expected == actually

## Tests for round
expect
    expected = Ok (
        [2, 4, 3, 5],
        [
            { items: [20, 23, 27, 26], ifFalse: ThrowToMonkey 3, ifTrue: ThrowToMonkey 2, operation: Mul 19, test: DivisibleBy 23 },
            { items: [2080, 25, 167, 207, 401, 1046], ifFalse: ThrowToMonkey 0, ifTrue: ThrowToMonkey 2, operation: Add 6, test: DivisibleBy 19 },
            { items: [], ifFalse: ThrowToMonkey 3, ifTrue: ThrowToMonkey 1, operation: MulOld, test: DivisibleBy 13 },
            { items: [], ifFalse: ThrowToMonkey 1, ifTrue: ThrowToMonkey 0, operation: Add 3, test: DivisibleBy 17 },
        ],
    )
    actually = example |> tryParseMonkeys |> Result.map (\monkeys -> round ([0, 0, 0, 0], monkeys))
    expected == actually

## Tests for runRounds
expect
    expected = Ok (
        [101, 95, 7, 105],
        [
            { items: [10, 12, 14, 26, 34], ifFalse: ThrowToMonkey 3, ifTrue: ThrowToMonkey 2, operation: Mul 19, test: DivisibleBy 23 },
            { items: [245, 93, 53, 199, 115], ifFalse: ThrowToMonkey 0, ifTrue: ThrowToMonkey 2, operation: Add 6, test: DivisibleBy 19 },
            { items: [], ifFalse: ThrowToMonkey 3, ifTrue: ThrowToMonkey 1, operation: MulOld, test: DivisibleBy 13 },
            { items: [], ifFalse: ThrowToMonkey 1, ifTrue: ThrowToMonkey 0, operation: Add 3, test: DivisibleBy 17 },
        ],
    )
    actually = example |> tryParseMonkeys |> Result.map (\monkeys -> runRounds 20 monkeys)
    expected == actually

## Tests for monkeyBusinessAfterRounds
expect
    expected = Ok 10605
    actually = example |> tryParseMonkeys |> Result.map (\monkeys -> monkeyBusinessAfterRounds 20 monkeys)
    expected == actually

## Tests for trySolvePuzzle
expect
    expected = Ok 10605
    actually = example |> trySolvePuzzle
    expected == actually

expect
    expected = Ok 62491
    actually = puzzle |> trySolvePuzzle
    expected == actually
