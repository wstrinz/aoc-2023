app "aoc23-daytemplate"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "daytemplate.txt" as input : Str,
    ]
    provides [main] to pf

part1 =
    "part1"

part2 =
    "part2"

main =
    results =
        [part1, part2]
        |> Str.joinWith ", "

    Stdout.line "Done!\n\(results)"
