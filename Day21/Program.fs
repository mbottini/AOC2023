#if INTERACTIVE
#r "/home/mike/fsharp/AOC2023/Prelude/bin/Debug/net8.0/Prelude.dll"
#r "nuget: Rationals"
#endif

open Prelude.Prelude
open Prelude.PogSeq

open Rationals

type Tile =
    | Wall
    | Plot
    | Start

    static member Parse c =
        match c with
        | '#' -> Wall
        | '.' -> Plot
        | 'S' -> Start
        | _ -> failwith "parse error"

type Direction =
    | N
    | S
    | E
    | W

type Coord =
    { row: int
      col: int }

    static member inline (+)({ row = r1; col = c1 }, { row = r2; col = c2 }) = { row = r1 + r2; col = c1 + c2 }

let directionVec dir =
    match dir with
    | N -> { row = -1; col = 0 }
    | S -> { row = 1; col = 0 }
    | E -> { row = 0; col = 1 }
    | W -> { row = 0; col = -1 }

type Board = array<array<Tile>>

let accessBoard (board: Board) { row = row; col = col } = board[row][col]

let normalizeMod x y = (x % y + y) % y

let accessBoard2 (board: Board) { row = row; col = col } =
    board[normalizeMod row board.Length][normalizeMod col board[0].Length]

let tryAccessBoard board { row = row; col = col } =
    Array.tryItem row board |> Option.bind (Array.tryItem col)

let neighbors coord =
    List.map directionVec [ N; S; E; W ] |> List.map ((+) coord)

let validNeighbors board coord =
    neighbors coord
    |> List.filter (accessBoard2 board >> (flip List.contains) [ Plot; Start ])

let nextStep board coords =
    Seq.collect (validNeighbors board) coords |> Set

let traverseSeq board start =
    iterate (nextStep board) (Set.singleton start)

let findStart board =
    seq {
        for row, arr in enumerate 0 board do
            for col, tile in enumerate 0 arr do
                let coord = { row = row; col = col }

                if accessBoard board coord = Start then
                    yield coord
    }
    |> Seq.head

let p2 n board =
    traverseSeq board (findStart board)
    |> Seq.take (n + 1)
    |> Seq.last
    |> Seq.length

let parseLine (s: string) = Seq.map Tile.Parse s |> Seq.toArray

let parseFile (ls: seq<string>) = Seq.map parseLine ls |> Seq.toArray

// This is now an interpolation problem. We get three valid terms from f(65, f(65+1*131), and f(65+2*131)).

type Polynomial =
    { coefs: list<Rational> }

    static member inline (+)({ coefs = c1 }, { coefs = c2 }) =
        { coefs = zipLongest (Rational 0) c1 c2 |> Seq.map ((uncurry (+))) |> Seq.toList }

    static member inline (~-) { coefs = coefs } = { coefs = List.map (~-) coefs }

    static member inline (-)(p1: Polynomial, p2: Polynomial) = p1 + (-p2)

    static member inline (*)({ coefs = c1 }, { coefs = c2 }) =
        let mutable arr: array<Rational> =
            List.replicate (c1.Length + c2.Length - 1) (Rational 0) |> Seq.toArray

        for p1, c1 in enumerate 0 c1 do
            for p2, c2 in enumerate 0 c2 do
                arr[p1 + p2] <- arr[p1 + p2] + c1 * c2

        { coefs = arr |> Seq.toList }

    member this.Apply(x: Rational) =
        match this with
        | { coefs = coefs } ->
            enumerate 0 coefs
            |> Seq.map (fun (pow, c) -> c * Rational.Pow(x, pow))
            |> Seq.fold (+) (Rational 0)

#nowarn "3391"

let basisPolynomial (points: seq<Rational * Rational>) =
    Seq.map fst points
    |> Seq.map (fun x -> { coefs = [ -x; Rational 1 ] })
    |> Seq.fold (fun x y -> x * y) { coefs = [ Rational 1 ] }

let barycentricWeights points =
    let helper x ys =
        List.map (fun y -> (Rational 1) / (Rational(x - y))) ys

    points
    |> List.map fst
    |> pickOne
    |> Seq.map ((uncurry helper) >> (Seq.fold (*) (Rational 1)))
    |> Seq.toList
    |> List.map (fun c -> { coefs = [ c ] })

let buildRootPoly points =
    List.map (fun (x, _) -> { coefs = [ -x; 1 ] }) points
    |> Seq.fold (*) { coefs = [ 1 ] }

(*
let lagrangePoly (points: list<int * int>) =
    let weights = barycentricWeights points |> List.map (fun w -> { coefs = [ w ] })

    let weights' =
        List.map2 (*) (List.map (snd >> (fun v -> { coefs = [ v ] })) points) weights

    let polys = pickOne points |> Seq.map snd |> buildRootPoly
    List.map2 (*) weights' polys
*)




[<EntryPoint>]
let main args =
    match args with
    | [| n; filename |] -> slurpOrStdin filename |> parseFile |> p2 (int n) |> printfn "%d"
    | _ -> printfn "main error"

    0
