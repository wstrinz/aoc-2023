app "aoc23-day2"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "day2.txt" as input : Str,
    ]
    provides [main] to pf


Turn : (Nat, Str)

Round : List Turn

Game : { gameId: Nat, rounds: List Round }

Condition : { conditionColor: Str, conditionCount: Nat }

gameIsPossible : Game, List Condition -> Bool
gameIsPossible = \game, conditions ->
    List.all conditions \{ conditionColor, conditionCount } ->
        game.rounds
        |> List.all \round ->
            round
            |> List.all \(turnCount, turnColor) ->
                turnColor != conditionColor || turnCount <= conditionCount


findMaxColorCount : Str, Game -> Nat
findMaxColorCount = \color, {rounds} ->
    rounds
    |> List.map \round ->
        round
        |> List.keepIf \(_, turnColor) ->
            turnColor == color
        |> List.map \(turnCount, _) ->
            turnCount
        |> List.max
        |> Result.withDefault 0
    |> List.max |> Result.withDefault 0

parseRound = \round ->
    round
    |> Str.split ","
    |> List.mapTry \detail ->
        when detail |> Str.split " " is
            [_, count, color] ->
                when Str.toNat count is
                    Ok countNum ->
                        Ok (countNum, color)

                    _ ->
                        Err NonIntegerCount

            _ ->
                Err BadGameDetails

parseGame = \line ->
    when line |> Str.split ":" is
        [gamePart, detailsPart] ->
            gameId =
                gamePart
                |> Str.split " "
                |> List.last
                |> Result.try Str.toNat

            roundDetails =
                detailsPart
                |> Str.split ";"
                |> List.mapTry parseRound

            when (gameId, roundDetails) is
                (Ok gameIdNum, Ok parsedRounds) ->
                    {gameId: gameIdNum, rounds: parsedRounds}

                _ ->
                    dbg gameId

                    crash "failed game id parse"

        otherwise ->
            dbg otherwise

            crash "failed game parse"

part2 =
    games : List Game
    games =
      input
      |> Str.split "\n"
      |> List.map parseGame

    maxValues =
        List.map games \game ->
            maxColors =
                ["red", "blue", "green"]
                    |> List.map \color ->
                        findMaxColorCount color game

            List.walk maxColors 0 \state, maxColor ->
                if state == 0 then
                    maxColor
                else
                    state * maxColor

    sum = List.sum maxValues

    Num.toStr sum

part1 =
    games =
      input
      |> Str.split "\n"
      |> List.map parseGame

    possibleGames =
        List.keepIf games \game ->
            gameIsPossible game [
                {conditionColor: "red", conditionCount: 12},
                {conditionColor: "blue", conditionCount: 14},
                {conditionColor: "green", conditionCount: 13},
            ]

    possibleSums
        = possibleGames
        |> List.map .gameId
        |> List.sum

    expect possibleSums == 2528

    Num.toStr possibleSums

main =
    results =
        [part1, part2]
        |> Str.joinWith ", "

    Stdout.line "Done!\n\(results)"
