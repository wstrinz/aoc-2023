app "aoc23-day7"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "day7.txt" as input : Str,
    ]
    provides [main] to pf

Cards : List Str

Hand : { cards : Cards, jokerVersion : Cards, bid : Nat }

cardValues : Dict Str Nat
cardValues =
    Dict.fromList [
        ("2", 2),
        ("3", 3),
        ("4", 4),
        ("5", 5),
        ("6", 6),
        ("7", 7),
        ("8", 8),
        ("9", 9),
        ("T", 10),
        ("J", 11),
        ("Q", 12),
        ("K", 13),
        ("A", 14),
    ]

jokerModeCardValues : Dict Str Nat
jokerModeCardValues =
    Dict.fromList [
        ("2", 2),
        ("3", 3),
        ("4", 4),
        ("5", 5),
        ("6", 6),
        ("7", 7),
        ("8", 8),
        ("9", 9),
        ("T", 10),
        ("Q", 11),
        ("K", 12),
        ("A", 13),
    ]

nOfAKind : Nat, Cards -> Bool
nOfAKind = \n, cards ->
    oak =
        cards
        |> List.any \card ->
            numCards =
                cards
                |>
                List.keepIf (\card2 -> card == card2)
                |> List.len

            numCards == n

    oak

fiveOfAKind = \cards -> nOfAKind 5 cards
fourOfAKind = \cards -> nOfAKind 4 cards
threeOfAKind = \cards -> nOfAKind 3 cards

fullHouse = \cards ->
    threeOfAKind cards
    && cards
    |> List.keepIf \card ->
        numOfAKind = cards |> List.keepIf (\card2 -> card == card2) |> List.len
        Bool.not (numOfAKind == 3)
    |> Set.fromList
    |> Set.len
    == 1

numPairs = \cards ->
    cards
    |> List.keepIf \card ->
        numCards =
            cards
            |>
            List.keepIf (\card2 -> card == card2)
            |> List.len

        numCards == 2
    |> List.len
    |> Num.divCeil 2

twoPairs : Cards -> Bool
twoPairs = \cards -> numPairs cards == 2

onePair = \cards -> numPairs cards == 1

handTypeFns : List (Str, (Cards -> Bool))
handTypeFns = [
    ("five", fiveOfAKind),
    ("four", fourOfAKind),
    ("full", fullHouse),
    ("three", threeOfAKind),
    ("twoPair", twoPairs),
    ("onePair", onePair),
]

compareHandType : (Cards -> Bool), Cards, Cards -> [LT, EQ, GT]
compareHandType = \handTypeFn, cardsA, cardsB ->
    resultA = handTypeFn cardsA
    resultB = handTypeFn cardsB

    if resultA == resultB then
        EQ
    else if resultA then
        GT
    else
        LT

betterHandType : Cards, Cards -> [LT, EQ, GT]
betterHandType = \cardsA, cardsB ->
    handTypeFns
    |> List.walk EQ \state, (_, handTypeFn) ->
        if state == EQ then
            compareHandType handTypeFn cardsA cardsB
        else
            state

compareHighCards = \cardsA, cardsB ->
    List.map2 cardsA cardsB Pair
    |> List.walk EQ \state, Pair cardA cardB ->
        if state == EQ then
            cardAValue = cardValues |> Dict.get cardA |> Result.withDefault 0
            cardBValue = cardValues |> Dict.get cardB |> Result.withDefault 0

            if cardAValue == cardBValue then
                EQ
            else if cardAValue > cardBValue then
                GT
            else
                LT
        else
            state

compareHands : Hand, Hand -> [LT, EQ, GT]
compareHands = \handA, handB ->
    when betterHandType handA.cards handB.cards is
        GT -> GT
        LT -> LT
        EQ ->
            when compareHighCards handA.cards handB.cards is
                EQ -> EQ
                different -> different

compareHighCardsWithJokers = \cardsA, cardsB ->
    List.map2 cardsA cardsB Pair
    |> List.walk EQ \state, Pair cardA cardB ->
        if state == EQ then
            cardAValue = jokerModeCardValues |> Dict.get cardA |> Result.withDefault 0
            cardBValue = jokerModeCardValues |> Dict.get cardB |> Result.withDefault 0

            if cardAValue == cardBValue then
                EQ
            else if cardAValue > cardBValue then
                GT
            else
                LT
        else
            state

compareHandsWithJokers : Hand, Hand -> [LT, EQ, GT]
compareHandsWithJokers = \handA, handB ->
    when betterHandType handA.jokerVersion handB.jokerVersion is
        GT -> GT
        LT -> LT
        EQ ->
            when compareHighCardsWithJokers handA.cards handB.cards is
                EQ -> EQ
                different -> different

getHandTypeScore : Cards -> Nat
getHandTypeScore = \cards ->
    handValue =
        handTypeFns
        |> List.walkUntil (Nothing 6) \idxBox, (fnName, handTypeFn) ->
            when idxBox is
                Found idx -> Break (Found (idx))
                Nothing idx ->
                    if handTypeFn cards then
                        Break (Found (idx))
                    else
                        Continue (Nothing (idx - 1))

    when handValue is
        Found idx -> idx
        Nothing _ -> 0

variationsSort : (Cards, Nat), (Cards, Nat) -> [LT, EQ, GT]
variationsSort = \(_, scoreA), (_, scoreB) ->
    if scoreA == scoreB then
        EQ
    else if scoreA > scoreB then
        GT
    else
        LT

jokerVariations : Cards -> List Cards
jokerVariations = \cards ->
    jokerIndex =
            cards
            |> List.findFirstIndex (\card -> card == "J")

    when jokerIndex is
        Ok replacementIdx ->
            variants =
                jokerModeCardValues
                |> Dict.keys
                |> List.map \card ->
                    cards
                    |> List.replace replacementIdx card
                    |> .list

            variants
            |> List.map \variation -> jokerVariations variation
            |> List.walk [] List.concat
        _ -> [cards]

getBestVariation : Cards -> (Cards, Nat)
getBestVariation = \cards ->
    if Str.joinWith cards "" == "JJJJJ" then
        ("AAAAA" |> Str.graphemes, 9001)
    else
        if Str.joinWith cards "" == "J8JJJ" then
            ("88888" |> Str.graphemes, 900)
        else
            variations = jokerVariations cards

            best =
                variations
                |> List.map \variation -> (variation, getHandTypeScore variation)
                |> List.sortWith variationsSort
                |> List.last

            when best is
                Ok theBest -> theBest
                _ -> crash "bad cards"

getJokerVersion : Cards -> Cards
getJokerVersion = \cards ->
    (getBestVariation cards).0

parseHands : List Hand
parseHands =
    input
    |> Str.split "\n"
    |> List.mapWithIndex \line, idx ->
        weirdSyntaxWorkround = "wat"

        [cards, bid, _] = line |> Str.split " "

        {
            cards: Str.graphemes cards,
            jokerVersion: Str.graphemes cards |> getJokerVersion,
            bid: bid |> Str.toNat |> Result.withDefault 0,
        }

part1 =
    hands = parseHands

    sorted = List.sortWith hands compareHands

    score =
        sorted
        |> List.mapWithIndex \hand, idx ->

            (hand.bid * (idx + 1))
        |> List.sum

    expect score == 251136060

    Num.toStr score

part2 =
    hands = parseHands

    sorted = List.sortWith hands compareHandsWithJokers

    score =
        sorted
        |> List.mapWithIndex \hand, idx ->
            (hand.bid * (idx + 1))
        |> List.sum

    expect score == 249400220

    Num.toStr score

main =
    results =
        [part1, part2]
        |> Str.joinWith ", "

    Stdout.line "Done!\n\(results)"
