app "aoc23-day5"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "day5.txt" as input : Str,
    ]
    provides [main] to pf

ResourceMap : { destStart: Nat, srcStart: Nat, length: Nat }

AgriMap : { source: Str, destination: Str, map: List ResourceMap }


parseSeeds = \seedSection ->
    parts = seedSection |> Str.split " "

    [_, .. as values] = parts

    values |> List.keepOks Str.toNat

parseSection : Str -> AgriMap
parseSection = \sectionString ->
    lines = sectionString |> Str.split "\n"

    [header, .. as ranges] = lines

    [headerInfo, _] = header |> Str.split " "

    [src, _, dest, _] = headerInfo |> Str.split "-"

    parsedRanges = ranges |> List.map \range ->
        when range |> Str.split " " |> List.keepOks Str.toNat is
            [destStart, srcStart, length] ->
                { destStart, srcStart, length }

            otherwise ->
                dbg otherwise

                crash "Invalid range"

    { source: src, destination: dest, map: parsedRanges }

parseInput : (List Nat, List AgriMap)
parseInput =
    sections = input |> Str.split "\n\n"

    when sections is
        [seedSection, .. as otherSections] ->
            parsedSeeds = parseSeeds seedSection

            parsedOthers = otherSections |> List.map parseSection

            (parsedSeeds, parsedOthers)

        otherwise ->
            dbg otherwise

            crash "Invalid input"

findLocation : Nat, List AgriMap, Dict Nat Nat -> (Nat, Dict Nat Nat)
findLocation = \seed, maps, seedCache ->
    when Dict.get seedCache seed is
        Ok cachedSeed ->
            (cachedSeed, seedCache)

        _ ->
            mapFor = \destination ->
                List.findFirst maps \map ->
                    map.destination == destination
                |> Result.map \r ->
                    r.map
                |> Result.withDefault []

            lookup = \number, mapDest ->
                mapFor mapDest
                |> List.findFirst \map ->
                    map.srcStart <= number && number < map.srcStart + map.length
                |> Result.map \entry ->
                    entry.destStart + (number - entry.srcStart)
                |> Result.withDefault number



            result =
                lookup seed "soil"
                |> lookup "fertilizer"
                |> lookup "water"
                |> lookup "light"
                |> lookup "temperature"
                |> lookup "humidity"
                |> lookup "location"

            (result, Dict.insert seedCache seed result )

part1 =
    parsedInput = parseInput

    (_, finalSeedDict) =
        parsedInput
        |> \(seeds, maps) ->
            seeds |> List.walk (0, Dict.empty {}) \(_, seedDict), seed ->
                findLocation seed maps seedDict


    result =
        Dict.toList finalSeedDict
        |> List.map .1
        |> List.min

    dbg result

    "part1"

part2 =
    parsedInput = parseInput

    seedLocations =
        parsedInput
        |> \(seeds, maps) ->
            seeds
            |> List.chunksOf 2
            |> List.map \seedChunk ->
                foo = "bar"

                [start, length, _] = seedChunk

                dbg (start, length)

                List.range { start: At start, end: Length length  }
                |> List.walk (0, Dict.empty {}) \(_, seedDict), seed ->
                    findLocation seed maps seedDict
        |> List.map .1


    dbg seedLocations

    "part2"

main =
    results =
        [part1, part2]
        |> Str.joinWith ", "

    Stdout.line "Done!\n\(results)"
