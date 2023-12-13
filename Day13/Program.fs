open Prelude.Prelude
open Prelude.PogSeq

open FSharp.Collections.ParallelSeq

type Direction =
    | Horizontal
    | Vertical

type Reflection = { direction: Direction; index: int }

let directionMultiplier dir =
    match dir with
    | Horizontal -> 100
    | Vertical -> 1

let toInt r =
    r.index * directionMultiplier r.direction

let parseFile ls =
    groupby (Seq.isEmpty >> not) ls
    |> Seq.filter fst
    |> Seq.map (snd >> Seq.map (Seq.toList) >> Seq.toList)

let diff l1 l2 =
    zip l1 l2 |> Seq.filter (uncurry (<>)) |> Seq.length

let rec reflectionHelper diffCount s1 s2 =
    match s1, s2 with
    | [], _ -> None
    | l :: ls, [] -> reflectionHelper diffCount ls (l :: s2)
    | l :: ls, _ ->
        if zip s1 s2 |> Seq.map (uncurry diff) |> Seq.sum = diffCount then
            Some(List.length s2)
        else
            reflectionHelper diffCount ls (l :: s2)

let findReflections diffCount ls =
    let helper l dir =
        match reflectionHelper diffCount l [] with
        | Some x -> Some { direction = dir; index = x }
        | None -> None

    List.zip [ ls; List.transpose ls ] [ Horizontal; Vertical ]
    |> List.collect (uncurry helper >> Option.toList)

let resolveDiffCount flag =
    match flag with
    | "-p1" -> 0
    | "-p2" -> 1
    | _ -> failwith "flag error"

[<EntryPoint>]
let main args =
    match args with
    | [| flag; filename |] ->
        slurpOrStdin filename
        |> parseFile
        |> PSeq.collect (findReflections (resolveDiffCount flag))
        |> Seq.map toInt
        |> Seq.sum
        |> printfn "%d"
    | _ -> failwith "main error"

    0
