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

let drawRay g (pos) =
    iterate (fun (row, col) -> (row + 1, col)) pos |> Seq.takeWhile (withinBounds g)

// S is an EW in my puzzle. This is a wretched hack
let goesLeft startPipe pipe =
    let leftPipes = [ NW; SW; EW ]
    List.contains pipe (leftPipes @ (if List.contains startPipe leftPipes then [ Start ] else []))

let goesRight startPipe pipe =
    let rightPipes = [ NE; SE; EW ]
    List.contains pipe (rightPipes @ (if List.contains startPipe rightPipes then [ Start ] else []))

let insidePipes g startPipe pipes pos =
    if Set.contains pos pipes then
        false
    else
        let ps =
            drawRay g pos
            |> Seq.filter (fun x -> Set.contains x pipes)
            |> Seq.map (uncurry (lookup g))
            |> Seq.toList

        let leftPipesCount = List.filter (goesLeft startPipe) ps |> List.length
        leftPipesCount % 2 = 1


let pointsContained g startPipe pipes =
    Seq.allPairs (seq { 0 .. Array.length g - 1 }) (seq { 0 .. Array.length g[0] - 1 })
    |> Seq.filter (insidePipes g startPipe pipes)

let deriveStart (pipes: list<Direction * (int * int)>) =
    match ((List.head >> fst >> behind) pipes, (List.tail >> List.head >> fst) pipes) with
    | (S, E) -> SE
    | (E, S) -> SE
    | (S, W) -> SW
    | (W, S) -> SW
    | (N, E) -> NE
    | (E, N) -> NE
    | (N, W) -> NW
    | (W, N) -> NW
    | (N, S) -> NS
    | (S, N) -> NS
    | (E, W) -> EW
    | (W, E) -> EW
    | _ -> failwith "Something went very wrong"

let p2 g =
    let pipes = p1 g

    pointsContained g ((Seq.toList >> deriveStart) pipes) ((Seq.map snd >> Set) pipes)
    |> Seq.length

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
