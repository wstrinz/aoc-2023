app "aoc23-day6"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "day6.txt" as input : Str,
    ]
    provides [main] to pf

solveQuadratic = \aNum, bNum, cNum ->
    a = aNum |> Num.toF64
    b = bNum |> Num.toF64
    c = cNum |> Num.toF64

    if aNum == 0 then
        NoRoots
    else
        discriminant = (Num.pow b 2) - 4 * a * c

        if discriminant < 0 then
            NoRoots
        else if Num.round discriminant == 0 then
            Single (Num.div (Num.neg b) (2 * a))
        else
            root1 =
                Num.div ((0 - b) + (discriminant |> Num.sqrt)) (2 * a)

            root2 =
                Num.div ((0 - b) - (discriminant |> Num.sqrt)) (2 * a)

            Double root1 root2

parseInput =
    foo = "123"

    [times, distances, _] =
        input
        |> Str.split "\n"
        |> List.map \line ->
            line
            |> Str.split " "
            |> List.keepOks Str.toNat

    List.map2 times distances \time, distance -> { time, distance }

findNumHoldtimes = \{ time, distance } ->
    roots = solveQuadratic 1 time distance

    when roots is
        NoRoots -> 0
        Single root -> root |> Num.round
        Double root1 root2 ->
            if Num.ceiling root1 == Num.floor root1 then
                roundedMin = root1 |> Num.neg |> Num.ceiling |> Num.add 1
                roundedMax = root2 |> Num.neg |> Num.floor |> Num.sub 1

                (roundedMax - roundedMin) + 1
            else
                roundedMin = root1 |> Num.neg |> Num.ceiling
                roundedMax = root2 |> Num.neg |> Num.floor

                (roundedMax - roundedMin) + 1

part1 =
    parsedInput = parseInput

    holdTimes = List.map parsedInput findNumHoldtimes

    result = holdTimes |> List.walk 1 Num.mul

    expect result == 505494

    Num.toStr result

part2 =
    parsedInput =
        parseInput
        |> List.walk ("", "") \(timeAcc, distAcc), { time, distance } ->

            (Str.joinWith [timeAcc, Num.toStr time] "", Str.joinWith [distAcc, Num.toStr distance] "")

    result =
        {
            time: Str.toNat parsedInput.0 |> Result.withDefault 0,
            distance: Str.toNat parsedInput.1 |> Result.withDefault 0,
        }
        |> findNumHoldtimes

    expect result == 23632299

    Num.toStr result

main =
    results =
        [part1, part2]
        |> Str.joinWith ", "

    Stdout.line "Done!\n\(results)"
