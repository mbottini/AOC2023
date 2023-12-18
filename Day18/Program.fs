open Prelude.Prelude
open Prelude.PogSeq

type Direction =
    | U
    | D
    | L
    | R

type Coord = { x: int64; y: int64 }

type Instruction = { dir: Direction; count: int64 }

let directAdd { x = x1; y = y1 } { x = x2; y = y2 } = { x = x1 + x2; y = y1 + y2 }
let scalarMultiply n { x = x; y = y } = { x = n * x; y = n * y }

let getVector dir =
    match dir with
    | U -> { x = 0L; y = 1L }
    | D -> { x = 0L; y = -1L }
    | L -> { x = -1L; y = 0L }
    | R -> { x = 1L; y = 0L }

let addInstruction c1 { dir = dir; count = count } =
    directAdd c1 (scalarMultiply count (getVector dir))

let determinant { x = x1; y = y1 } { x = x2; y = y2 } = x1 * y2 - x2 * y1

let diff { x = x1; y = y1 } { x = x2; y = y2 } = abs (x2 - x1) + abs (y2 - y1)

let perimeter ps =
    Seq.pairwise ps |> Seq.map (uncurry diff) |> Seq.sum

let shoelaceArea ps =
    Seq.pairwise ps
    |> Seq.map (uncurry determinant)
    |> Seq.sum
    |> abs
    |> fun x -> x / 2L

let filledArea ps =
    let perim = perimeter ps
    let area = shoelaceArea ps
    area + perim / 2L + 1L


let parseDir s =
    match s with
    | "U"
    | "3" -> U
    | "D"
    | "1" -> D
    | "L"
    | "2" -> L
    | "R"
    | "0" -> R
    | _ -> failwith "parse error"

let parseInstruction s =
    let matcher (m: System.Text.RegularExpressions.Match) =
        { dir = parseDir m.Groups[1].Value
          count = int64 m.Groups[2].Value }

    reMatch """(\w)\s+(\d+)\s+\(#\w{6}\)""" matcher s

let parseInstruction2 s =
    let matcher (m: System.Text.RegularExpressions.Match) =
        { dir = parseDir m.Groups[2].Value
          count = System.Convert.ToInt64(m.Groups[1].Value, 16) }

    reMatch """\w\s+\d+\s+\(#(\w{5})(\w)\)""" matcher s

let origin = { x = 0L; y = 0L }

let vecsToPoints ps = Seq.scan addInstruction origin ps

let parseFile parser ls =
    Seq.map parser ls |> vecsToPoints |> Seq.toList

let resolveParser flag =
    match flag with
    | "-p1" -> parseInstruction
    | "-p2" -> parseInstruction2
    | _ -> failwith "flag error"

[<EntryPoint>]
let main args =
    match args with
    | [| flag; filename |] ->
        slurpOrStdin filename
        |> parseFile (resolveParser flag)
        |> filledArea
        |> printfn "%d"
    | _ -> printfn "main error"

    0
