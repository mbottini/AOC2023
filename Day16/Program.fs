open Prelude.Prelude
open Prelude.PogSeq

open FSharp.Collections.ParallelSeq

type Tile =
    | Space
    | EWSplitter
    | NSSplitter
    | NESWMirror
    | NWSEMirror

type Direction =
    | N
    | S
    | E
    | W

type Beam =
    { direction: Direction
      coordinate: int * int }

let getVector dir =
    match dir with
    | N -> (-1, 0)
    | S -> (1, 0)
    | E -> (0, 1)
    | W -> (0, -1)

let splitNS dir =
    match dir with
    | N -> [ N ]
    | S -> [ S ]
    | W
    | E -> [ N; S ]

let splitEW dir =
    match dir with
    | N
    | S -> [ E; W ]
    | W -> [ W ]
    | E -> [ E ]

let reflectNESW dir =
    match dir with
    | S -> W
    | E -> N
    | W -> S
    | N -> E

let reflectNWSE dir =
    match dir with
    | S -> E
    | E -> S
    | W -> N
    | N -> W

let directAdd (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

let parseChar c =
    match c with
    | '.' -> Space
    | '|' -> NSSplitter
    | '-' -> EWSplitter
    | '/' -> NESWMirror
    | '\\' -> NWSEMirror
    | _ -> failwith "parse error"

let parseLine (l: string) = Seq.map parseChar l |> Seq.toArray

let parseFile (ls: seq<string>) = Seq.map parseLine ls |> Seq.toArray

let accessBoard board row col =
    Array.tryItem row board |> Option.bind (Array.tryItem col)

let validLocation board row col =
    accessBoard board row col |> Option.isSome

let processBeam beam tile =
    match tile with
    | Space ->
        [ { direction = beam.direction
            coordinate = directAdd (getVector beam.direction) beam.coordinate } ]
    | NWSEMirror ->
        [ { direction = reflectNWSE beam.direction
            coordinate = directAdd (getVector (reflectNWSE beam.direction)) beam.coordinate } ]
    | NESWMirror ->
        [ { direction = reflectNESW beam.direction
            coordinate = directAdd (getVector (reflectNESW beam.direction)) beam.coordinate } ]
    | NSSplitter ->
        splitNS beam.direction
        |> List.map (fun d ->
            { direction = d
              coordinate = directAdd (getVector d) beam.coordinate })
    | EWSplitter ->
        splitEW beam.direction
        |> List.map (fun d ->
            { direction = d
              coordinate = directAdd (getVector d) beam.coordinate })

let nextStep
    (board: array<array<Tile>>)
    (visited: Set<Direction * (int * int)>)
    (beams: list<Beam>)
    : Set<Direction * (int * int)> * list<Beam> =

    let newBeams =
        seq {
            for b in beams do
                match uncurry (accessBoard board) b.coordinate with
                | Some tile when not (Set.contains (b.direction, b.coordinate) visited) -> yield! processBeam b tile
                | _ -> ()
        }
        |> Seq.filter (_.coordinate >> (uncurry (validLocation board)))
        |> Seq.toList

    (Set.union visited (List.map (fun b -> (b.direction, b.coordinate)) beams |> Set)), newBeams

let allSteps board startCoord =
    iterate
        (uncurry (nextStep board))
        (Set.empty,
         [ { direction = E
             coordinate = startCoord } ])
    |> Seq.takeWhile (snd >> List.isEmpty >> not)

let allTop (board: array<array<Tile>>) =
    seq { 0 .. board[0].Length - 1 } |> Seq.map (fun col -> (0, col))

let allLeft (board: array<array<Tile>>) =
    seq { 0 .. board.Length - 1 } |> Seq.map (fun row -> (row, 0))

let allBottom (board: array<array<Tile>>) =
    seq { 0 .. board[0].Length - 1 } |> Seq.map (fun col -> (board.Length - 1, col))

let allRight (board: array<array<Tile>>) =
    seq { 0 .. board.Length - 1 } |> Seq.map (fun row -> (row, board[0].Length - 1))

let allStartingPoints board =
    [ allTop; allBottom; allLeft; allRight ] |> Seq.collect (fun f -> f board)

let p1 board start =
    allSteps board start
    |> Seq.map (snd >> Set)
    |> Seq.fold (Set.union) Set.empty
    |> Set.map _.coordinate
    |> Set.count

let p2 board =
    allStartingPoints board
    |> enumerate 1
    |> Seq.map snd
    |> PSeq.map (p1 board)
    |> PSeq.max

[<EntryPoint>]
let main args =
    match args with
    | [| "-p1"; filename |] -> slurpOrStdin filename |> parseFile |> (flip p1) (0, 0) |> printfn "%d"
    | [| "-p2"; filename |] -> slurpOrStdin filename |> parseFile |> p2 |> printfn "%d"
    | _ -> printfn "main error"

    0
