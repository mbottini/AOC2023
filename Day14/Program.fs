open Prelude.Prelude
open Prelude.PogSeq

type Direction =
    | N
    | S
    | E
    | W

type Tile =
    | Wall
    | Boulder
    | Floor

    static member ToString t =
        match t with
        | Wall -> "#"
        | Boulder -> "O"
        | Floor -> "."

    static member Parse c =
        match c with
        | '#' -> Wall
        | 'O' -> Boulder
        | '.' -> Floor
        | _ -> failwith "parse error"

type Board = list<list<Tile>>

let boardRepresentation b =
    List.map (List.map Tile.ToString >> System.String.Concat) b
    |> List.toArray
    |> fun arr -> System.String.Join('\n', arr)

let parseFile ls : Board =
    Seq.map (Seq.map Tile.Parse >> Seq.toList) ls |> Seq.toList

let rotateGravity dir =
    match dir with
    | W -> id
    | E -> List.map List.rev
    | N -> List.transpose
    | S -> List.rev >> List.transpose

let rotateBack dir =
    match dir with
    | W -> id
    | E -> List.map List.rev
    | N -> List.transpose
    | S -> List.transpose >> List.rev

let tiltLine l =
    groupby ((=) Wall) l |> Seq.collect (snd >> List.sort) |> Seq.toList

let measureLoadLine l =
    enumerate 1 l |> Seq.filter (snd >> ((=) Boulder)) |> Seq.map fst |> Seq.sum

let measureLoad b =
    rotateGravity S b |> List.map measureLoadLine |> List.sum

let tilt dir lss =
    rotateGravity dir lss |> List.map tiltLine |> rotateBack dir

let doCycle: Board -> Board = [ N; W; S; E ] |> List.map tilt |> (List.fold (>>) id)

let boardCycle b =
    let helper (st, curr) f = (Set.add curr st, f curr)

    let cyc =
        Seq.scan helper (Set.empty, b) (repeat doCycle)
        |> Seq.pairwise
        |> Seq.takeWhile (fun ((st1, _), (st2, _)) -> st1 <> st2)
        |> Seq.map (fst >> snd)
        |> Seq.toList

    let head = List.last cyc |> doCycle
    let idx = List.findIndex ((=) head) cyc
    (idx, List.skip idx cyc)

let part1 = tilt N >> measureLoad

let part2 b =
    let skip, cyc = boardCycle b

    cyc[(1000000000L - (int64 skip)) % (List.length cyc |> int64) |> int]
    |> measureLoad

let resolveTilter flag =
    match flag with
    | "-p1" -> part1
    | "-p2" -> part2
    | _ -> failwith "flag error"

[<EntryPoint>]
let main args =
    match args with
    | [| flag; filename |] -> slurpOrStdin filename |> parseFile |> resolveTilter flag |> printfn "%d"
    | _ -> failwith "main error"

    0
