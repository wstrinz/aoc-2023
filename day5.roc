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

findLocation : Nat, List AgriMap -> Nat
findLocation = \seed, maps ->
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

    result

part1 =
    parsedInput = parseInput

    result =
        parsedInput
        |> \(seeds, maps) ->
            seeds |> List.map \seed ->
                findLocation seed maps
        |> List.min

    dbg result

    "part1"

part2 =
    parsedInput = parseInput

    nDrops = 6

    dbg "Dropping \(nDrops |> Num.toStr) seeds"
    
    seedLocations =
        parsedInput
        |> \(seeds, maps) ->
            seeds
            |> List.chunksOf 2
            |> List.dropFirst nDrops
            |> List.takeFirst 1
            |> List.map \seedChunk ->
                foo = "bar"

                [start, length, _] = seedChunk

                loc =
                    List.range { start: At start, end: Length length  }
                    |> List.map \seed ->
                        findLocation seed maps
                    |> List.min

                when loc is
                    Ok min ->
                        dbg (start, min)

                        min

                    otherwise ->
                        dbg otherwise

                        crash "Invalid min"

    dbg seedLocations

    dbg List.min seedLocations

    "part2"

main =
    results =
        [part2]
        |> Str.joinWith ", "

    Stdout.line "Done!\n\(results)"
