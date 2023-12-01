let stringToDigitList = 
    List.zip
        ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine";
                 "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"] 
        [1; 2; 3; 4; 5; 6; 7; 8; 9;
                 1; 2; 3; 4; 5; 6; 7; 8; 9]

let seqToCalibration sq = 
    Seq.toArray sq |> fun arr -> arr.[0] * 10 + arr.[arr.Length - 1]

let parseLine s = 
    Seq.filter System.Char.IsAsciiDigit s
        |> Seq.map (string >> int)
        |> seqToCalibration
        
let rec parseLine2Helper (s: string) =
    match (s, stringToDigitList |> List.filter (fun (s2, _) -> s.StartsWith s2)) with
    | ("", _) -> Seq.empty
    | (_, []) -> parseLine2Helper (s.Substring 1)
    | (_, (s2, repl) :: _) -> seq {
        yield repl
        yield! parseLine2Helper (s.Substring 1)
    }
    
let parseLine2 s = 
    parseLine2Helper s |> seqToCalibration

let rec repeatedly f =
    seq {
        yield f ()
        yield! repeatedly f
    } 
let lines (stream: System.IO.Stream) =
    let sr = new System.IO.StreamReader(stream)
    repeatedly sr.ReadLine |> Seq.takeWhile (fun line -> line <> null)

let resolveFilename (s: string): System.IO.Stream =
    if s = "-" 
        then System.Console.OpenStandardInput(1)
        else System.IO.File.OpenRead s
        
let resolveParser (s: string): string -> int =
    match s with
    | "-p1" -> parseLine
    | "-p2" -> parseLine2
    | _ -> raise (new System.Exception "Invalid parse flag")

[<EntryPoint>]
let main2 args =
    try
        match args with
        | [|part; filename|] -> 
            resolveFilename filename |>
                lines |>
                Seq.map (resolveParser part) |>
                Seq.sum |>
                printfn "%d"
                0
        | _ -> 
            printfn "Usage: ./prog -p<1|2> <filename or ->"
            1
    with
    | _ as e -> 
        printfn "%s" e.Message
        2
