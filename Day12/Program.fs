open Prelude.Prelude
open Prelude.PogSeq

type Spring =
    | Good
    | Bad
    | Unknown

type Line =
    { springs: list<Spring>
      blocks: list<int> }

let parseChar c =
    match c with
    | '.' -> Good
    | '#' -> Bad
    | '?' -> Unknown
    | _ -> failwith "parse error"

let parseNums s =
    split ',' s |> Seq.map int |> Seq.toList

let parseLine multiplier s =
    match split ' ' s with
    | [| springs; nums |] ->
        { springs =
            intersperse "?" (Seq.replicate multiplier springs)
            |> Seq.concat
            |> System.String.Concat
            |> Seq.map parseChar
            |> Seq.toList
          blocks =
            intersperse "," (Seq.replicate multiplier nums)
            |> Seq.concat
            |> System.String.Concat
            |> parseNums }
    | _ -> failwith "parse error"

let rec springCombos (memo: Map<list<Spring> * list<int>, int64> ref) springs blocks =
    let dropper sps bs =
        match sps, bs with
        | s :: rest, bs when List.contains s [ Unknown; Good ] -> springCombos memo rest bs
        | [], [] -> 1L
        | _ -> 0L

    let rec keeper sps bs =
        match sps, bs with
        | [], (0 :: remBlocks) -> springCombos memo sps remBlocks
        | s :: rest, 0 :: remBlocks when List.contains s [ Unknown; Good ] -> springCombos memo rest remBlocks
        | s :: rest, block :: remBlocks when List.contains s [ Unknown; Bad ] -> keeper rest (block - 1 :: remBlocks)
        | _ -> 0L

    match Map.tryFind (springs, blocks) memo.Value with
    | Some x -> x
    | None ->
        let total = dropper springs blocks + keeper springs blocks
        memo.Value <- Map.add (springs, blocks) total memo.Value
        total

let lineCombos l =
    springCombos (ref Map.empty) l.springs l.blocks

let resolveMultiplier flag =
    match flag with
    | "-p1" -> 1
    | "-p2" -> 5
    | _ -> failwith "flag error"

[<EntryPoint>]
let main args =
    match args with
    | [| flag; filename |] ->
        slurpOrStdin filename
        |> Seq.map (parseLine (resolveMultiplier flag))
        |> Seq.map lineCombos
        |> Seq.sum
        |> printfn "%d"
    | _ -> failwith "main error"

    0
