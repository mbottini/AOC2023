type Direction =
    | L
    | R

let parseDirection c =
    match c with
    | 'L' -> L
    | 'R' -> R
    | _ -> failwith "parse error"

type GraphNode =
    { label: string
      left: string
      right: string }

type Graph = System.Collections.Generic.Dictionary<string, GraphNode>

let parseLine s =
    (System.Text.RegularExpressions.Regex """(\w+) = \((\w+), (\w+)\)""").Match s
    |> fun m ->
        { label = m.Groups[1].Value
          left = m.Groups[2].Value
          right = m.Groups[3].Value }

let createGraph nodes : Graph =
    let m = new System.Collections.Generic.Dictionary<string, GraphNode>()
    Seq.iter (fun gn -> m.Add(gn.label, gn)) nodes
    m

let uncons xs =
    Seq.tryHead xs |> Option.map (fun x -> x, Seq.tail xs)

let parseFile ls =
    match uncons ls with
    | Some(x, xs) -> (Seq.map parseDirection x |> Seq.toList, xs |> Seq.map parseLine |> createGraph)
    | None -> failwith "parse error"

let traverseGraph' pred dirs (graph: System.Collections.Generic.Dictionary<string, GraphNode>) start =
    let traverser curr d =
        match d with
        | L -> graph[curr].left
        | R -> graph[curr].right in

    Seq.scan traverser start dirs |> Seq.takeWhile (pred >> not) |> Seq.length

let rec cycle xs =
    seq {
        yield! xs
        yield! cycle xs
    }

let traverseGraph1 dirs graph =
    traverseGraph' ((=) "ZZZ") (cycle dirs) graph "AAA"

let uncurry f = fun (x, y) -> f x y

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

let lcm x y =
    x * y / (System.Numerics.BigInteger.GreatestCommonDivisor(x, y))

let traverseGraph2 dirs (graph: System.Collections.Generic.Dictionary<string, GraphNode>) =
    let pred (s: string) = s.EndsWith "Z"

    graph.Keys
    |> Seq.filter (fun s -> s.EndsWith "A")
    |> Seq.map (fun s -> traverseGraph' pred (cycle dirs) graph s)
    |> Seq.map System.Numerics.BigInteger
    |> Seq.fold lcm (1 |> System.Numerics.BigInteger)

let resolveTraverser flag =
    match flag with
    | "-p1" -> uncurry traverseGraph1 >> System.Numerics.BigInteger
    | "-p2" -> uncurry traverseGraph2
    | _ -> failwith "flag error"

[<EntryPoint>]
let main args =
    match args with
    | [| flag; filename |] ->
        resolveFilename filename
        |> lines
        |> parseFile
        |> resolveTraverser flag
        |> printfn "%A"
    | _ -> printfn "%s" "main error"

    0
