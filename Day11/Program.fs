open Prelude.Prelude
open Prelude.PogSeq

let emptySpace sq = Seq.forall ((=) '.') sq

let resolveGalaxy1 ls =
    let lst = Seq.toList ls

    let emptyRows =
        enumerate 0 lst |> Seq.filter (snd >> emptySpace) |> Seq.map fst |> Set

    let emptyCols =
        Seq.transpose lst
        |> enumerate 0
        |> Seq.filter (snd >> emptySpace)
        |> Seq.map fst
        |> Set

    seq {
        for (y, row) in enumerate 0 lst do
            let curr =
                seq {
                    for (x, c) in enumerate 0 row do
                        if Set.contains x emptyCols then
                            yield c
                            yield c
                        else
                            yield c
                }

            if Set.contains y emptyRows then
                yield curr
                yield curr
            else
                yield curr
    }
    |> Seq.map Seq.toArray
    |> Seq.toArray

let coords arr =
    seq {
        for (y, row) in enumerate 0 arr do
            for (x, c) in enumerate 0 row do
                if c = '#' then
                    yield (y, x) // row, col for the purposes of checking the array
    }
    |> Seq.toArray

let allSegments points =
    Seq.allPairs points points |> Seq.filter (fun (p1, p2) -> p1 < p2) |> Seq.toList

let manhattan ((y1, x1), (y2, x2)) = (abs (y2 - y1)) + (abs (x2 - x1))

let p1 (ls: seq<string>) =
    resolveGalaxy1 ls |> coords |> allSegments |> Seq.map manhattan |> Seq.sum

[<EntryPoint>]
let main args =
    match args with
    | [| flag; filename |] -> slurpOrStdin filename |> p1 |> printfn "%A"

    0
