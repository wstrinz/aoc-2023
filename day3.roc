app "aoc23-day3"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "day3.txt" as input : Str,
    ]
    provides [main] to pf

Point : { x : Nat, y : Nat }

StartPoint : [NotSet, Point Point]

State : {
    currentNumbers : List Str,
    numberMap : Dict Point Str,
    symbolMap : Dict Point Str,
    startPoint : StartPoint,
}

digits = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

initialState : State
initialState = {
    currentNumbers: [],
    numberMap: Dict.empty {},
    symbolMap: Dict.empty {},
    startPoint: NotSet,
}

pointsAroundPoint : Point -> Set Point
pointsAroundPoint = \{ x, y } ->
    when (x, y) is
        (0, 0) ->
            Set.fromList [
                { x, y: y + 1 },
                { x: x + 1, y },
                { x: x + 1, y: y + 1 },
            ]

        (0, _) ->
            Set.fromList [
                { x, y: y - 1 },
                { x, y: y + 1 },
                { x: x + 1, y: y - 1 },
                { x: x + 1, y },
                { x: x + 1, y: y + 1 },
            ]

        (_, 0) ->
            Set.fromList [
                { x: x - 1, y },
                { x: x - 1, y: y + 1 },
                { x, y: y + 1 },
                { x: x + 1, y },
                { x: x + 1, y: y + 1 },
            ]

        _ ->
            Set.fromList [
                { x: x - 1, y: y - 1 },
                { x: x - 1, y },
                { x: x - 1, y: y + 1 },
                { x, y: y - 1 },
                { x, y: y + 1 },
                { x: x + 1, y: y - 1 },
                { x: x + 1, y },
                { x: x + 1, y: y + 1 },
            ]

pointsAroundNumber : Str, Point -> Set Point
pointsAroundNumber = \number, { x, y } ->
    number
    |> Str.graphemes
    |> List.mapWithIndex \_, index ->
        pointsAroundPoint { x: x + index, y }
    |> List.walk (Set.empty {}) Set.union

parseCharacter : State, Str, Point -> State
parseCharacter = \state, char, { x, y } ->
    finishNumberIfStarted =
        when state.startPoint is
            NotSet ->
                state

            Point point ->
                { state &
                    currentNumbers: [],
                    numberMap: Dict.insert state.numberMap point (Str.joinWith state.currentNumbers ""),
                    startPoint: NotSet,
                }

    when char is
        "." ->
            finishNumberIfStarted

        digitChar if List.contains digits digitChar ->
            when state.startPoint is
                NotSet ->
                    { state & startPoint: Point { x, y }, currentNumbers: [digitChar] }

                Point _ ->
                    { state & currentNumbers: List.append state.currentNumbers digitChar }

        symbolChar ->
            { finishNumberIfStarted & symbolMap: Dict.insert state.symbolMap { x, y } symbolChar }

parseLine = \state, line, yCoord ->
    line
    |> Str.graphemes
    |> List.walkWithIndex state \lineState, char, xCoord ->
        parseCharacter lineState char { x: xCoord, y: yCoord }

parseInput =
    input
    |> Str.split "\n"
    |> List.walkWithIndex initialState parseLine

part1 =
    parsedInput =
        parseInput

    pointsAround =
        parsedInput.numberMap
        |> Dict.toList
        |> List.map \(startPoint, number) -> (number, pointsAroundNumber number startPoint)

    numbersBySymbols =
        pointsAround
        |> List.keepIf \(_, points) ->
            points
            |> Set.toList
            |> List.any \point -> Dict.contains parsedInput.symbolMap point
        |> List.map \(number, _) -> number

    result =
        numbersBySymbols
        |> List.keepOks Str.toNat
        |> List.sum

    expect result == 520135

    Num.toStr result

part2 =
    parsedInput =
        parseInput

    allNumbersPoints =
        parsedInput.numberMap
        |> Dict.toList
        |> List.map \(startPoint, number) ->
            allPoints =
                number
                |> Str.graphemes
                |> List.mapWithIndex \_, index -> { x: startPoint.x + index, y: startPoint.y }

            (number, allPoints)

    asteriskPoints =
        parsedInput.symbolMap
        |> Dict.toList
        |> List.keepIf \(_, symbol) -> symbol == "*"
        |> List.map \(point, _) -> point

    asteriskPointsAndNumbersAdjacency =
        asteriskPoints
        |> List.map \asteriskPoint ->
            pointsAround = pointsAroundPoint asteriskPoint

            numberPointsAround =
                allNumbersPoints
                |> List.keepIf \(_, allPoints) ->
                    allPoints
                    |> List.any \point -> Set.contains pointsAround point

            (asteriskPoint, numberPointsAround)

    validGears =
        asteriskPointsAndNumbersAdjacency
        |> List.keepIf \(_, numberPointsAround) -> List.len numberPointsAround == 2

    expect result == 72514855

    result =
        validGears
        |> List.map \(_, numberPointsAround) ->
            numberPointsAround
            |> List.map \(number, _) -> number
            |> List.keepOks Str.toNat
            |> List.walk 1 Num.mul
        |> List.sum

    Num.toStr result

main =
    results =
        [part1, part2]
        |> Str.joinWith ", "

    Stdout.line "Done!\n\(results)"
