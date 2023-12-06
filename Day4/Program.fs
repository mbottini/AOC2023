type Card =
    { id: int
      winners: Set<int>
      actual: Set<int> }

let split (s: string) (delim: char) =
    s.Split(
        delim,
        System.StringSplitOptions.RemoveEmptyEntries
        ||| System.StringSplitOptions.TrimEntries
    )

let parseHeader (s: string) =
    (System.Text.RegularExpressions.Regex """Card\s+(\d+)""").Match s
    |> fun m -> m.Groups[1].Value |> int

let parseNumbers (s: string) = split s ' ' |> Array.map int

let parseRest (id: int) (s: string) =
    match split s '|' with
    | [| winners; actual |] ->
        { id = id
          winners = parseNumbers winners |> Set
          actual = parseNumbers actual |> Set }
    | _ -> failwith "Parse error"


let parseLine (s: string) : Card =
    match split s ':' with
    | [| header; rest |] -> parseRest (parseHeader header) rest
    | _ -> failwith "Parse error"

let scoreCard (c: Card) : int =
    1 <<< ((Set.intersect c.winners c.actual) |> Seq.length) >>> 1

let countWinners (c: Card) : int =
    Set.intersect c.winners c.actual |> Seq.length

let rec incrementNextVals (n: int) (v: int) (lst: list<int>) : list<int> =
    match (n, lst) with
    | 0, lst -> lst
    | n, [] -> List.replicate n v
    | n, x :: xs -> x + v :: incrementNextVals (n - 1) v xs

let rec playGameHelper (acc: list<int>) (cs: list<Card>) =
    match (cs, acc) with
    | (c :: rest, x :: xs) ->
        seq {
            yield x
            yield! playGameHelper (incrementNextVals (countWinners c) x xs) rest
        }
    | _ -> Seq.empty

let processScores: seq<Card> -> int = Seq.map scoreCard >> Seq.sum

let processCardCounts (cs: seq<Card>) : int =
    let lst = Seq.toList cs
    playGameHelper (List.replicate lst.Length 1) lst |> Seq.sum


let rec repeatedly f =
    seq {
        yield f ()
        yield! repeatedly f
    }

let lines (stream: System.IO.Stream) =
    let sr = new System.IO.StreamReader(stream)
    repeatedly sr.ReadLine |> Seq.takeWhile (fun line -> line <> null)

let resolveFilename (s: string) : System.IO.Stream =
    if s = "-" then
        System.Console.OpenStandardInput 1
    else
        System.IO.File.OpenRead s

let resolveProcessor (s: string) : seq<Card> -> int =
    match s with
    | "-p1" -> processScores
    | "-p2" -> processCardCounts
    | _ -> failwith "flag error"



[<EntryPoint>]
let main args =
    try
        match args with
        | [| flag; filename |] ->
            resolveFilename filename
            |> lines
            |> Seq.map parseLine
            |> resolveProcessor flag
            |> printfn "%A"

            0
        | _ -> failwith "args error"
    with _ as e ->
        printfn "%A" e.Message
        2
