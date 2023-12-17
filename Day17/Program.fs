#if INTERACTIVE
#r "nuget: FSharpx.Collections"
#r "/home/mike/fsharp/AOC2023/Prelude/bin/Debug/net8.0/Prelude.dll"
#endif

open Prelude.Prelude
open Prelude.PogSeq

type Direction =
    | N
    | S
    | E
    | W

type Node = list<Direction * (int * int)>

let behind dir =
    match dir with
    | N -> S
    | S -> N
    | W -> E
    | E -> W

let otherDirs dir =
    [ N; S; E; W ] |> List.filter (behind >> ((<>) dir))

let getVector dir =
    match dir with
    | N -> (-1, 0)
    | S -> (1, 0)
    | W -> (0, -1)
    | E -> (0, 1)

let directAdd (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

let parseLine (s: string) =
    Seq.map (string >> System.Int32.Parse) s |> Seq.toArray

let parseFile (ls: seq<string>) = Seq.map parseLine ls |> Seq.toArray

let tryGetLocation board row col =
    Array.tryItem row board |> Option.bind (Array.tryItem col)

let getLocation (board: array<array<int>>) row col = board[row][col]

let validLocation board row col =
    tryGetLocation board row col |> Option.isSome

type Heap = FSharpx.Collections.Heap<int * Node>

let emptyHeap: Heap = FSharpx.Collections.Heap.empty false

let push = FSharpx.Collections.Heap.insert

let pushAll xs hp = Seq.fold (flip push) hp xs

let pop = FSharpx.Collections.Heap.tryUncons

let neighborCoord coord dir = directAdd (getVector dir) coord

let getChain sq =
    match uncons sq with
    | Some(x, xs) -> cons x (Seq.takeWhile ((=) x) xs)
    | _ -> []

let getChainNode n = Seq.map fst n |> getChain

let getChainNodeLength n = getChainNode n |> Seq.length

let validDirections start ``end`` n =
    let chainLength = getChainNodeLength n

    match n with
    | (d, _) :: _ when chainLength >= start && chainLength < ``end`` -> otherDirs d
    | (d, _) :: _ when chainLength < start -> [ d ]
    | (d, _) :: _ -> otherDirs d |> List.filter ((<>) d)
    | _ -> failwith "empty list should never happen!"

let neighbors start ``end`` board n =
    match n with
    | (_, coord) :: _ ->
        validDirections start ``end`` n
        |> List.map (fun d -> (d, neighborCoord coord d) :: n)
        |> List.filter (List.head >> snd >> (uncurry (validLocation board)))
    | _ -> failwith "empty list should never happen!"

let accessHeadValue board n =
    List.head n |> snd |> uncurry (getLocation board)

let rec nextStep neighborFunc board visited (neighborHeap: FSharpx.Collections.Heap<int * Node>) =
    match pop neighborHeap with
    | Some((cost, (((_, coord) :: _) as n)), hp') ->
        if Map.containsKey (getChainNode n |> Seq.toList, coord) visited then
            nextStep neighborFunc board visited hp'
        else
            Some(
                (Map.add (getChainNode n |> Seq.toList, coord) cost visited,
                 pushAll
                     (neighborFunc board n
                      |> List.map (fun n' -> (accessHeadValue board n' + cost, n')))
                     hp')
            )
    | _ -> None

let initialHeap board =
    [ [ (E, (0, 1)) ]; [ (S, (1, 0)) ] ]
    |> List.map (fun n -> (accessHeadValue board n, n))
    |> (flip pushAll) emptyHeap

let (initialMap: Map<(List<Direction> * (int * int)), int>) = Map.empty

let findSmallestPath start ``end`` board =
    iterate (Option.bind (uncurry (nextStep (neighbors start ``end``) board))) (Some(initialMap, initialHeap board))
    |> Seq.takeWhile Option.isSome
    |> Seq.collect Option.toList
    |> Seq.last
    |> fst
    |> Map.toSeq
    |> Seq.filter (fun ((_, coord), _) -> coord = (board.Length - 1, board[0].Length - 1))
    |> Seq.map snd
    |> Seq.min

let p1 = findSmallestPath 1 3
let p2 = findSmallestPath 4 10

#if INTERACTIVE
let board = slurpOrStdin "Day17/test1.txt" |> parseFile
let hp = initialHeap board

let f: option<Map<List<Direction> * (int * int), int> * Heap> -> option<Map<List<Direction> * (int * int), int> * Heap> =
    Option.bind (uncurry (nextStep2 board))
#endif

[<EntryPoint>]
let main args =
    match args with
    | [| "-p1"; filename |] -> slurpOrStdin filename |> parseFile |> p1 |> printfn "%d"
    | [| "-p2"; filename |] -> slurpOrStdin filename |> parseFile |> p2 |> printfn "%d"
    | _ -> printfn "main error"

    0
