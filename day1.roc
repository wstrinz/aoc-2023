app "aoc23-day1"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [
        pf.Stdout,
        "day1.txt" as input : Str,
    ]
    provides [main] to pf

digitPairs = [
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9),
]

wordDigitFor = \current, character ->
    combinedString = List.append current character |> Str.joinWith ""

    List.findFirst digitPairs (\(word, _) -> Str.endsWith combinedString word)
    |> Result.map (\(_, digit) -> digit)

digitsAndWordDigits = \state, character ->
    when Str.toNat character is
        Ok digit ->
            { state & digits: List.append state.digits digit, current: [] }

        Err _ ->
            when wordDigitFor state.current character is
                Ok digit ->
                    { state & digits: List.append state.digits digit, current: List.append state.current character }

                Err _ ->
                    { state & current: List.append state.current character }


firstAndListDigitsString = \digits ->
    first = List.get digits 0
    last = List.last digits

    [first, last]
    |> List.map (\r -> Result.withDefault r 0)
    |> List.map Num.toStr
    |> Str.joinWith ""

getDigitsAndWordDigits = \line ->
    initialState = { current: [], digits: [] }

    line
    |> Str.graphemes
    |> List.walk initialState digitsAndWordDigits
    |> .digits
    |> firstAndListDigitsString

getDigits = \line ->
    digits =
        line
        |> Str.graphemes
        |> List.map Str.toNat
        |> List.dropIf Result.isErr
        |> List.map (\r -> Result.withDefault r 0)

    firstAndListDigitsString digits

part1 =
    result =
        Str.split input "\n"
        |> List.map getDigits
        |> List.map Str.toNat
        |> List.map (\r -> Result.withDefault r 0)
        |> List.sum
        |> Num.toStr

    expect result == "54605"

    result

part2 =
    result =
        Str.split input "\n"
        |> List.map getDigitsAndWordDigits
        |> List.map Str.toNat
        |> List.map (\r -> Result.withDefault r 0)
        |> List.sum
        |> Num.toStr

    expect result == "55429"

    result

expect wordDigitFor ["e","i","g","h","t","h","r","e"] "e" == Ok 3
expect digitsAndWordDigits { current: ["e","i","g","h","t","h","r","e"], digits: [] } "e" == { current: ["e","i","g","h","t","h","r","e", "e"], digits: [3] }

main =
    results =
        [part1, part2]
        |> Str.joinWith ", "

    Stdout.line "Done!\n\(results)"
