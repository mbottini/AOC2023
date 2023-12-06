type Race = { time: int64; distance: int64 }

let runRace (totalTime: int64) (buttonTime: int64) = buttonTime * (totalTime - buttonTime)

let buttonCombinations (r: Race) =
    seq { 0L .. r.time }
    |> Seq.map (runRace r.time)
    |> Seq.filter (fun x -> x > r.distance)
    |> Seq.length
    |> int64

let raceCombinations (rs: list<Race>) : int64 =
    List.map buttonCombinations rs |> List.fold (*) 1L

let getInts (line: string) : list<int> =
    (System.Text.RegularExpressions.Regex """\d+""").Matches line
    |> Seq.map (fun m -> m.Value |> int)
    |> Seq.toList

let parseFile (lines: list<string>) =
    match lines with
    | [ times; distances ] ->
        List.zip (getInts times) (getInts distances)
        |> List.map (fun (t, d) -> { time = t; distance = d })
    | _ -> failwith "parse error"

let digits (s: string) : seq<int> =
    Seq.filter System.Char.IsAsciiDigit s |> Seq.map (string >> int)

let digitsToBigNum (sq: seq<int>) : int64 =
    Seq.fold (fun acc x -> 10L * acc + int64 x) 0L sq

let parseFile2 (lines: list<string>) =
    let parser = digits >> digitsToBigNum

    match lines with
    | [ time; distance ] ->
        { time = parser time
          distance = parser distance }
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

let resolveParser (s: string) =
    match s with
    | "-p1" -> parseFile >> raceCombinations
    | "-p2" -> parseFile2 >> buttonCombinations
    | _ -> failwith "flag error"

[<EntryPoint>]
let main args =
    try
        match args with
        | [| flag; filename |] ->
            resolveFilename filename
            |> lines
            |> Seq.toList
            |> resolveParser flag
            |> printfn "%d"

            0
        | _ ->
            printfn "Usage: ./prog -p<1|2> <filename>"
            1
    with _ as e ->
        printfn "%A" e.Message
        2
