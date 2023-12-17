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

let singleton x = emptyHeap |> push x

let pushAll xs hp = Seq.fold (flip push) hp xs

let pop = FSharpx.Collections.Heap.tryUncons

let neighborCoord coord dir = directAdd (getVector dir) coord

let validDirections n =
    match n with
    | (d1, _) :: (d2, _) :: (d3, _) :: _ when d1 = d2 && d2 = d3 -> otherDirs d1 |> List.filter ((<>) d1)
    | (d1, _) :: _ -> otherDirs d1
    | _ -> failwith "empty list should never happen!"

let getChain sq =
    match uncons sq with
    | Some(x, xs) -> cons x (Seq.takeWhile ((=) x) xs)
    | _ -> []

let getChainNode n = Seq.map fst n |> getChain

let getChainNodeLength n = getChainNode n |> Seq.length

let validDirections2 n =
    let chainLength = getChainNodeLength n

    match n with
    | (d, _) :: _ when chainLength >= 4 && chainLength < 10 -> otherDirs d
    | (d, _) :: _ when chainLength < 4 -> [ d ]
    | (d, _) :: _ -> otherDirs d |> List.filter ((<>) d)
    | _ -> failwith "empty list should never happen!"


let neighbors board n =
    match n with
    | (_, coord) :: _ ->
        validDirections n
        |> List.map (fun d -> (d, neighborCoord coord d) :: n)
        |> List.filter (List.head >> snd >> (uncurry (validLocation board)))
    | _ -> failwith "empty list should never happen!"

let neighbors2 board n =
    match n with
    | (_, coord) :: _ ->
        validDirections2 n
        |> List.map (fun d -> (d, neighborCoord coord d) :: n)
        |> List.filter (List.head >> snd >> (uncurry (validLocation board)))
    | _ -> failwith "empty list should never happen!"

let accessHeadValue board n =
    List.head n |> snd |> uncurry (getLocation board)

let getPredDirs count (n: Node) = List.truncate count n |> List.map fst

let rec nextStep board visited (neighborHeap: FSharpx.Collections.Heap<int * Node>) =
    match pop neighborHeap with
    | Some((cost, (((_, coord) :: _) as n)), hp') ->
        if Map.containsKey (getPredDirs 3 n, coord) visited then
            nextStep board visited hp'
        else
            Some(
                (Map.add (getPredDirs 3 n, coord) cost visited,
                 pushAll (neighbors board n |> List.map (fun n' -> (accessHeadValue board n' + cost, n'))) hp')
            )
    | _ -> None

let rec nextStep3 (board: array<array<int>>) visited neighborHeap =
    match pop neighborHeap with
    | Some((cost, (((_, coord) :: _) as n)), _) when
        coord = (board.Length - 1, board[0].Length - 1) && getChainNodeLength n >= 4
        ->
        cost
    | Some((cost, (((_, coord) :: _) as n)), hp') ->
        if Map.containsKey (getChainNode n |> Seq.toList, coord) visited then
            nextStep3 board visited hp'
        else
            nextStep3
                board
                (Map.add (getChainNode n |> Seq.toList, coord) cost visited)
                (pushAll (neighbors2 board n |> List.map (fun n' -> (accessHeadValue board n' + cost, n'))) hp')

    | _ -> failwith "heap exhaustion without hitting the end"

let rec nextStep2 (board: array<array<int>>) visited (neighborHeap: FSharpx.Collections.Heap<int * Node>) =
    match pop neighborHeap with
    | Some((cost, (((_, coord) :: _) as n)), hp') ->
        if Map.containsKey (getChainNode n |> Seq.toList, coord) visited then
            nextStep2 board visited hp'
        else
            Some(
                (Map.add (getChainNode n |> Seq.toList, coord) cost visited,
                 pushAll (neighbors2 board n |> List.map (fun n' -> (accessHeadValue board n' + cost, n'))) hp')
            )
    | _ -> None

let initialHeap board =
    [ [ (E, (0, 1)) ]; [ (S, (1, 0)) ] ]
    |> List.map (fun n -> (accessHeadValue board n, n))
    |> (flip pushAll) emptyHeap

let (initialMap: Map<(List<Direction> * (int * int)), int>) = Map.empty

let p1 board =
    iterate (Option.bind (uncurry (nextStep board))) (Some(initialMap, initialHeap board))
    |> Seq.takeWhile Option.isSome
    |> Seq.collect Option.toList
    |> Seq.last
    |> fst
    |> Map.toSeq
    |> Seq.filter (fun ((_, coord), _) -> coord = (board.Length - 1, board[0].Length - 1))
    |> Seq.map snd
    |> Seq.min

(*
let p2 board =
    nextStep2 board initialMap (initialHeap board)
*)

let p2 board =
    iterate (Option.bind (uncurry (nextStep2 board))) (Some(initialMap, initialHeap board))
    |> Seq.takeWhile Option.isSome
    |> Seq.collect Option.toList
    |> Seq.last
    |> fst
    |> Map.toSeq
    |> Seq.filter (fun ((n, coord), _) -> coord = (board.Length - 1, board[0].Length - 1) && List.length n >= 4)
    |> Seq.map snd
    |> Seq.min


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
