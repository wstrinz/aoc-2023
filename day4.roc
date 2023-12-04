app "aoc23-day4"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "day4.txt" as input : Str,
    ]
    provides [main] to pf

Card : { id : Nat, cardNumbers : List Nat, winningNumbers : List Nat, matches : Nat }

findMatches : Card -> Nat
findMatches = \card ->
    card.cardNumbers
    |> List.keepIf \cardNumber ->
        List.contains card.winningNumbers cardNumber
    |> List.len

parseInput =
    input
    |> Str.split "\n"
    |> List.map \line ->
        cardId = line |> Str.split ":" |> List.first |> Result.try (\r -> Str.split r " " |> List.keepOks Str.toNat |> List.last)
        ticketNumbers = line |> Str.split ":" |> List.last |> Result.map (\r -> Str.split r "|")
        winningNums = ticketNumbers |> Result.try List.first |> Result.map (\r -> Str.split r " ") |> Result.map (\r -> List.keepOks r Str.toNat)
        cardNums = ticketNumbers |> Result.try List.last |> Result.map (\r -> Str.split r " ") |> Result.map (\r -> List.keepOks r Str.toNat)

        when (cardId, cardNums, winningNums) is
            (Ok id, Ok cardNumbers, Ok winningNumbers) ->
                card = { id, cardNumbers, winningNumbers, matches: 0 }

                { card & matches: findMatches card }

            other ->
                dbg
                    other

                crash "Invalid input"

buildCardDict = \cards ->
    cards
    |> List.map \card -> (card.id, card)
    |> Dict.fromList

part1 =
    parsedInput =
        parseInput

    result =
        parsedInput
        |> List.map \card ->
            if card.matches == 0 then
                0
            else
                Num.powInt 2 (card.matches - 1)
        |> List.sum
        |> Num.toStr

    expect result == "17782"

    result

getCopiesWon : Card, Dict Nat Card -> List Card
getCopiesWon = \card, cardDict ->
    if card.matches == 0 then
        []
    else
        cardsWon =
            List.range { start: After card.id, end: Length card.matches }
            |> List.keepOks \cardId ->
                cardDict |> Dict.get cardId

        List.map cardsWon \copy ->
            getCopiesWon copy cardDict
        |> List.walk [] \acc, copies ->
            List.concat acc copies
        |> List.concat cardsWon

# getCopiesWon : Card, Dict Nat Card, CardCountCache -> (CardTreeResult, CardCountCache)
# getCopiesWon = \card, cardDict, countCache ->
#     when Dict.get countCache card.id is
#         Ok cachedCount ->
#             (Cached cachedCount, countCache)

#         _ ->
#             if card.matches == 0 then
#                 (Full [], countCache)
#             else
#                 cardsWon =
#                     List.range { start: After card.id, end: Length card.matches }
#                     |> List.keepOks \cardId ->
#                         cardDict |> Dict.get cardId

#                 allCards : List CardTreeResult
#                 allCards =
#                     List.map cardsWon \copy ->
#                         getCopiesWon copy cardDict countCache
#                     |> List.walk [] \acc, (copiesResult, copiesCache) ->
#                         subResult =
#                             when copiesResult is
#                                 Full copies ->
#                                     (Full copies)
#                                 Cached copiesCachCount ->
#                                     (Cached copiesCachCount)

#                         List.append acc subResult

#                 newTotal =
#                     allCards
#                     |> List.walk 0 \acc, cardTreeResult ->
#                         when cardTreeResult is
#                             Full cards ->
#                                 List.len cards + acc
#                             Cached cachedCount ->
#                                 cachedCount + acc

#                 (Cached newTotal, countCache |> Dict.insert card.id newTotal)
part2 =
    inputCards =
        parseInput

    cardDict =
        inputCards |> buildCardDict

    wonCopies =
        inputCards
        |> List.map \card ->
            getCopiesWon card cardDict
        |> List.walk [] \acc, copies ->
            List.concat acc copies

    result =
        wonCopies
        |> List.concat inputCards
        |> List.len
        |> Num.toStr

    result

main =
    results =
        [part1, part2]
        |> Str.joinWith ", "

    Stdout.line "Done!\n\(results)"
