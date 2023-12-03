type Game = {balls: Map<string, int>}

type GameSet = {
    id: int
    games: list<Game>
}

let getOrDefault k def m =
    match Map.tryFind k m with
    | Some(v) -> v
    | None -> def

let diffMaps (possible: Map<string, int>) (observed: Map<string, int>) =
    Map.map (fun key  value -> value - (getOrDefault key 0 observed)) possible
    
let unionMaps (m1: Map<'k, int>) (m2: Map<'k, int>) =
    seq {
        for (k, v) in Seq.zip m1.Keys m1.Values do
            yield (k, max v (getOrDefault k 0 m2))
        for (k, v) in Seq.zip m2.Keys m2.Values |> 
            Seq.filter (fun (k, _) -> not (m1.ContainsKey k)) do
                yield (k, v)
    } |> Map

let actual = Map [("red", 12); ("green", 13); ("blue", 14)]

let validGame possible (observed: Game) = 
    Seq.forall (fun x -> x >= 0) (diffMaps possible observed.balls).Values
        
let validGameSet possible observed =
    observed.games |> Seq.forall (validGame possible)
    
let unionGame games = 
    Seq.map (_.balls) games |> Seq.fold unionMaps Map.empty

let split (s: string) (delim: char) =
    s.Split (
        delim, 
        System.StringSplitOptions.RemoveEmptyEntries ||| System.StringSplitOptions.TrimEntries)

let parseEntry (s: string) =
    split s ' '
        |> fun arr -> (arr.[1], int arr.[0])
let parseGame (s: string): Game =
    split s ','
        |> Seq.map parseEntry |> Map |> fun x -> {balls = x}
        
let parseID (s: string) =
    System.Text.RegularExpressions.Regex("""Game (\d+)""").Match(s).Groups.[1].Value |>
        int

let parseGameSet (s: string): GameSet =
    match split s ':' with
    | [|identity: string; gameSetString: string|] -> 
        {id = parseID identity; games = split gameSetString ';' |> Array.map parseGame |> Array.toList}
    | _ -> raise (new System.ArgumentException "Parse error")
    
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
        then System.Console.OpenStandardInput 1
        else System.IO.File.OpenRead s

let processValidGameSets =
    Seq.filter (validGameSet actual) >>
    Seq.map _.id >>
    Seq.sum

let processSetPowers =
    Seq.map (_.games >> unionGame >> _.Values >> Seq.fold (*) 1) >>
    Seq.sum

let resolveProcessor (s: string): seq<GameSet> -> int =
    match s with
    | "-p1" -> processValidGameSets
    | "-p2" -> processSetPowers
    | _ -> raise (System.ArgumentException "Invalid parse flag")


[<EntryPoint>]    
let main args = 
    try
        match args with
        | [|flag; filename|] -> 
            resolveFilename filename |> 
            lines |> 
            Seq.map parseGameSet |>
            resolveProcessor flag |>
            printfn "%A"
            0
        | _ -> 
            printfn "Usage: <prog> -<p1|p2> <filename | ->"
            1
    with
    | _ as e -> 
        printfn "%A" e.Message
        2
        