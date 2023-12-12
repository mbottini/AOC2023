open Prelude.Prelude
open Prelude.PogSeq

type Spring =
    | Good
    | Bad
    | Unknown

type Block = list<Spring>

type Line =
    { blocks: list<Block>
      contigs: list<int> }

let parseChar c =
    match c with
    | '.' -> Good
    | '#' -> Bad
    | '?' -> Unknown
    | _ -> failwith "parse error"

let parseNums s =
    split ',' s |> Seq.map int |> Seq.toList

let parseBlocks s =
    Seq.map parseChar s
    |> groupby ((<>) Good)
    |> Seq.filter fst
    |> Seq.map snd
    |> Seq.toList

let parseLine s =
    match split ' ' s with
    | [| springs; nums |] ->
        { blocks = parseBlocks springs
          contigs = parseNums nums }
    | _ -> failwith "parse error"

let rec blockCombos block contigs =
    let helper bs cs =
        match bs with
        | Unknown :: rest -> blockCombos rest cs
        | [] -> [ ([], cs) ]
        | _ -> []

    helper block contigs
    @ match contigs with
      | c :: cs ->
          if c <= List.length block then
              match List.skip c block with
              | Unknown :: xs -> List.map (fun (ns, ys) -> (c :: ns, ys)) (blockCombos xs cs)
              | [] -> [ ([ c ], cs) ]
              | _ -> []
          else
              []
      | [] -> []

let rec blockCombosMultiple blocks contigs =
    match (blocks, contigs) with
    | [], [] -> [ [] ]
    | (b :: bs), contigs ->
        seq {
            for (lst, contigs') in blockCombos b contigs do
                yield! Seq.map ((@) lst) (blockCombosMultiple bs contigs')
        }
        |> Seq.toList
    | _ -> []

let lineCombos l = blockCombosMultiple l.blocks l.contigs

[<EntryPoint>]
let main args =
    match args with
    | [| flag; filename |] ->
        slurpOrStdin filename
        |> Seq.map parseLine
        |> Seq.map (lineCombos >> Seq.length)
        |> Seq.sum
        |> printfn "%d"
    | _ -> failwith "main error"

    0
