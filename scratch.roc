app "aoc23-scratch"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [
        pf.Stdout,
    ]
    provides [main] to pf

# listOfStrings : List Str
listOfStrings = ["123", "456", "abc", "789"]

# convertAndFilterValidNumbers : List Str -> List Int
convertAndFilterValidNumbers = \list ->
    List.mapTry list Str.toI32

# resultingListOfNumbers : List Int
resultingListOfNumbers = convertAndFilterValidNumbers listOfStrings

main =
    resultingListOfNumbers
