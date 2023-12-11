open Prelude.Prelude
open Prelude.PogSeq

type Galaxy =
    { emptyRows: Set<int64>
      emptyCols: Set<int64>
      points: array<int64 * int64> }

let emptySpace sq = Seq.forall ((=) '.') sq

let resolveGalaxy (ls: seq<string>) : Galaxy =
    let lst = Seq.toList ls

    let emptyRows =
        enumerate 0 lst
        |> Seq.filter (snd >> emptySpace)
        |> Seq.map (fst >> int64)
        |> Set

    let emptyCols =
        Seq.transpose lst
        |> enumerate 0
        |> Seq.filter (snd >> emptySpace)
        |> Seq.map (fst >> int64)
        |> Set

    let points =
        seq {
            for (y, row) in enumerate 0 lst do
                for (x, c) in enumerate 0 row do
                    if c = '#' then
                        yield (int64 y, int64 x)
        }
        |> Seq.toArray

    { emptyRows = emptyRows
      emptyCols = emptyCols
      points = points }

let allSegments g =
    Seq.allPairs g.points g.points
    |> Seq.filter (fun (p1, p2) -> p1 < p2)
    |> Seq.toList

let getRange x y = seq { min x y + 1L .. max x y }

let manhattan g multiplier ((y1, x1), (y2, x2)) =
    (getRange y1 y2
     |> Seq.map (fun row -> if Set.contains row g.emptyRows then multiplier else 1L)
     |> Seq.sum)
    + (getRange x1 x2
       |> Seq.map (fun col -> if Set.contains col g.emptyCols then multiplier else 1L)
       |> Seq.sum)


let processSeq multiplier (ls: seq<string>) =
    let g = resolveGalaxy ls
    allSegments g |> Seq.map (manhattan g multiplier) |> Seq.sum

let resolveMultiplier flag =
    match flag with
    | "-p1" -> 2L
    | "-p2" -> 1000000L
    | _ -> failwith "flag error"


[<EntryPoint>]
let main args =
    match args with
    | [| flag; filename |] -> slurpOrStdin filename |> processSeq (resolveMultiplier flag) |> printfn "%d"
    | _ -> printfn "%s" "main error"

    0
