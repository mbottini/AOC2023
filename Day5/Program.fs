type Range =
    { destination: int64
      start: int64
      ``end``: int64 }

type RangeMap = { label: string; ranges: list<Range> }

type Seed = { start: int64; ``end``: int64 }

let lookupRange (r: Range) (x: int64) : option<int64> =
    if x >= r.start && x <= r.``end`` then
        Some(r.destination + (x - r.start))
    else
        None

let lookupRangeMap (rm: RangeMap) (x: int64) : int64 =
    match List.map (fun r -> lookupRange r x) rm.ranges |> List.filter Option.isSome with
    | Some y :: _ -> y
    | _ -> x

let chainRange (rms: list<RangeMap>) (x: int64) : int64 =
    List.fold (fun acc rm -> lookupRangeMap rm acc) x rms

let lookupRange2 (r: Range) (s: Seed) : option<Seed> =
    let newStart = max r.start s.start
    let newEnd = min r.``end`` s.``end``

    if newStart > newEnd then
        None
    else
        Some
            { start = r.destination + (newStart - r.start)
              ``end`` = r.destination + (newEnd - r.start) }

let lookupRangeMap2 (rm: RangeMap) (seeds: list<Seed>) =
    List.allPairs rm.ranges seeds
    |> List.map (fun (r, s) -> lookupRange2 r s)
    |> List.collect (Option.toList)

let chainRange2 (rms: List<RangeMap>) (seeds: list<Seed>) =
    List.fold (fun acc rm -> lookupRangeMap2 rm acc) seeds rms


let split (s: string) (delim: char) =
    s.Split(
        delim,
        System.StringSplitOptions.RemoveEmptyEntries
        ||| System.StringSplitOptions.TrimEntries
    )

let parseRange (xs: array<int64>) : Range =
    match xs with
    | [| dest; start; distance |] ->
        { destination = dest
          start = start
          ``end`` = start + distance }
    | _ -> failwith "parse error"

let parseLine s =
    split s ' ' |> Array.map int64 |> parseRange

let parseHeader s =
    (System.Text.RegularExpressions.Regex "(.*) map:").Match s
    |> fun m -> m.Groups[1].Value

let parseMap (xs: list<string>) : RangeMap =
    match xs with
    | y :: ys ->
        { label = parseHeader y
          ranges = List.map parseLine ys }
    | _ -> failwith "parse error"

let parseSeeds (line: string) : list<int64> =
    line.Substring 7 |> (fun s -> split s ' ') |> Array.map int64 |> Array.toList

let parseSeeds2 (line: string) : list<Seed> =
    line.Substring 7
    |> (fun s -> split s ' ')
    |> Seq.map int64
    |> Seq.chunkBySize 2
    |> Seq.map (fun xs ->
        { start = xs[0]
          ``end`` = xs[0] + xs[1] })
    |> Seq.toList

let rec groupbyHelper f curr acc xs =
    match xs with
    | [] ->
        if List.isEmpty acc then
            Seq.empty
        else
            Seq.singleton (List.rev acc)
    | y :: ys ->
        if f y = curr then
            groupbyHelper f curr (y :: acc) ys
        else
            seq {
                yield List.rev acc
                yield! groupbyHelper f (f y) [ y ] ys
            }

let groupby f xs =
    match xs with
    | [] -> Seq.empty
    | y :: ys -> groupbyHelper f (f y) [ y ] ys

let parseMaps lines =
    groupby (fun (s: string) -> s.Length = 0) lines
    |> Seq.filter (fun lst -> lst[0].Length <> 0)
    |> Seq.map parseMap
    |> Seq.toList

let parseFile lines =
    match lines with
    | seeds :: rest -> (parseSeeds seeds, parseMaps rest)
    | _ -> failwith "parse error"

let parseFile2 lines =
    match lines with
    | seeds :: rest -> (parseSeeds2 seeds, parseMaps rest)
    | _ -> failwith "parse error"

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

let processPart1 (lines: list<string>) : int64 =
    parseFile lines
    |> (fun (seeds, rms) -> List.map (chainRange rms) seeds)
    |> List.min

let processPart2 (lines: list<string>) : int64 =
    parseFile2 lines
    |> (fun (seeds: list<Seed>, rms) -> chainRange2 rms seeds)
    |> List.map _.start
    |> List.min

let resolveProcessor (s: string) : list<string> -> int64 =
    match s with
    | "-p1" -> processPart1
    | "-p2" -> processPart2
    | _ -> failwith "flag error"

[<EntryPoint>]
let main args =
    try
        match args with
        | [| flag; filename |] ->
            resolveFilename filename
            |> lines
            |> Seq.toList
            |> resolveProcessor flag
            |> printfn "%A"

            0
        | _ -> failwith "args error"
    with _ as e ->
        printfn "%A" e.Message
        2
