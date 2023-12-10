open Prelude.Prelude
open Prelude.PogSeq

type Pipe =
    | NS
    | EW
    | NW
    | NE
    | SW
    | SE
    | X
    | Start

type Direction =
    | N
    | S
    | E
    | W

let behind dir =
    match dir with
    | N -> S
    | S -> N
    | E -> W
    | W -> E

let left dir =
    match dir with
    | N -> W
    | W -> S
    | S -> E
    | E -> N

let right = left >> behind

let incrementor dir row col =
    match dir with
    | N -> (row - 1, col)
    | S -> (row + 1, col)
    | W -> (row, col - 1)
    | E -> (row, col + 1)

let findDirection (r1, c1) (r2, c2) =
    match (r2 - r1, c2 - c1) with
    | (1, 0) -> N
    | (-1, 0) -> S
    | (0, -1) -> W
    | (0, 1) -> E
    | _ -> failwith "direction error"

let getNextDir dir pipe =
    match dir, pipe with
    | N, NS -> N
    | S, NS -> S
    | E, EW -> E
    | W, EW -> W
    | S, NW -> W
    | E, NW -> N
    | S, NE -> E
    | W, NE -> N
    | N, SW -> W
    | E, SW -> S
    | N, SE -> E
    | W, SE -> S
    | _ -> failwith "Invalid pipe connection"

let lookup (g: array<array<Pipe>>) row col = g[row][col]

let nextPipe g currentDir pos =
    let currentPipe = uncurry (lookup g) pos
    let nextDir = getNextDir currentDir currentPipe
    (nextDir, uncurry (incrementor nextDir) pos)

let allPipes g startPos =
    let tryGo (dir: Direction) : bool =
        try
            nextPipe g dir (uncurry (incrementor dir) startPos) |> ignore
            true
        with _ ->
            false

    let d = List.filter tryGo [ N; E; S; W ] |> List.head
    let d2 = List.filter tryGo [ N; E; S; W ] |> List.last |> behind

    iterate (uncurry (nextPipe g)) (d, uncurry (incrementor d) startPos)
    |> Seq.takeWhile (snd >> (=) startPos >> not)
    |> cons (d2, startPos)


let parsePipe c =
    match c with
    | '|' -> NS
    | '-' -> EW
    | 'J' -> NW
    | 'L' -> NE
    | '7' -> SW
    | 'F' -> SE
    | 'S' -> Start
    | _ -> X

let parseFile ls =
    Seq.map (Seq.map parsePipe >> Seq.toArray) ls |> Seq.toArray

let findStart graph =
    let grid = Seq.map (enumerate 0) graph |> enumerate 0

    seq {
        for (row, xs) in grid do
            for (col, x) in xs do
                if x = Start then
                    yield (row, col)
    }
    |> Seq.tryHead

let p1 g =
    let startPos = findStart g |> _.Value
    allPipes g startPos

let allNeighbors (row, col) =
    [ (row - 1, col); (row + 1, col); (row, col - 1); (row, col + 1) ]

let withinBounds g (row, col) =
    Array.tryItem row g |> Option.bind (Array.tryItem col) |> Option.isSome

let leftNeighbor dir pos = uncurry (incrementor (left dir)) pos

let rightNeighbor dir pos = uncurry (incrementor (right dir)) pos

let floodFill g pipes start =
    let nextStep curr =
        Seq.filter ((flip Set.contains) pipes >> not) curr
        |> Seq.collect allNeighbors
        |> Seq.filter (withinBounds g)
        |> Seq.fold (flip Set.add) curr
        |> (flip Set.difference) pipes

    iterate nextStep start |> Seq.pairwise |> Seq.find (uncurry (=)) |> snd

let floodLeft g =
    let sq = p1 g
    floodFill g (Seq.map snd sq |> Set) (Seq.map (uncurry leftNeighbor) sq |> Set)

let floodRight g =
    let sq = p1 g
    floodFill g (Seq.map snd sq |> Set) (Seq.map (uncurry rightNeighbor) sq |> Set)

let p2 g =
    min (Seq.length (floodRight g)) (Seq.length (floodLeft g))

[<EntryPoint>]
let main args =
    match args with
    | [| "-p1"; filename |] ->
        slurpOrStdin filename
        |> parseFile
        |> p1
        |> Seq.length
        |> fun x -> x / 2 |> printfn "%A"
    | [| "-p2"; filename |] -> slurpOrStdin filename |> parseFile |> p2 |> printfn "%A"

    | _ -> printfn "%s" "main error"

    0
