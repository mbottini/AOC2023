type Number =
    { value: int
      row: int
      column: {| _start: int; _end: int |} }

type Symbol =
    { value: string; row: int; column: int }

let parseNumbers (row: int) (line: string) : list<Number> =
    (System.Text.RegularExpressions.Regex """\d+""").Matches line
    |> Seq.map (fun m ->
        { Number.value = int m.Value
          row = row
          column =
            {| _start = m.Index
               _end = m.Index + m.Value.Length - 1 |} })
    |> Seq.toList

let parseSymbols (row: int) (line: string) : list<Symbol> =
    (System.Text.RegularExpressions.Regex """[^\.\d]""").Matches line
    |> Seq.map (fun m ->
        { Symbol.value = m.Value
          row = row
          column = m.Index })
    |> Seq.toList

let parseSeq (lines: list<string>) : list<list<Number>> * list<list<Symbol>> =
    (List.mapi parseNumbers lines, List.mapi parseSymbols lines)

let prependAndAppend x xs =
    seq {
        yield x
        yield! xs
        yield x
    }

let concat3 (xs, ys, zs) = Seq.concat [ xs; ys; zs ]

let triplewise xs =
    let ys = prependAndAppend [] (Seq.map List.singleton xs)
    Seq.zip3 ys (Seq.tail ys) ((Seq.tail >> Seq.tail) ys) |> Seq.map concat3

let isAdjacent (number: Number) (symbol: Symbol) =
    abs (number.row - symbol.row) <= 1
    && number.column._start - 1 <= symbol.column
    && number.column._end + 1 >= symbol.column

let validNumber (symbols: list<Symbol>) (number: Number) = Seq.exists (isAdjacent number) symbols

let filterNumbers (numbers: seq<list<Number>>) (symbols: seq<list<Symbol>>) =
    let candidateSymbols = triplewise symbols |> Seq.map (Seq.concat >> Seq.toList)

    Seq.map2 (fun ns cs -> Seq.filter (validNumber cs) ns) numbers candidateSymbols
    |> Seq.concat

let processSymbols (numbers: seq<list<Number>>) (symbols: seq<list<Symbol>>) =
    let candidateNumbers = triplewise numbers |> Seq.map (Seq.concat >> Seq.toList)

    seq {
        for lst, nums in Seq.zip symbols candidateNumbers do
            for sym in lst do
                let adjacentNums: list<Number> = List.filter (fun num -> isAdjacent num sym) nums

                if adjacentNums.Length = 2 then
                    yield (List.map (fun (n: Number) -> n.value) adjacentNums)
    }

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

let part1 (lines: list<string>) =
    let numbers, symbols = parseSeq lines
    filterNumbers numbers symbols |> Seq.map _.value

let part2 (lines: list<string>) =
    let numbers, symbols = parseSeq lines
    processSymbols numbers symbols |> Seq.map (List.fold (*) 1)

let resolveProcessor (s: string) : list<string> -> seq<int> =
    match s with
    | "-p1" -> part1
    | "-p2" -> part2
    | _ -> raise (System.ArgumentException "Invalid parse flag")


[<EntryPoint>]
let main args =
    try
        match args with
        | [| flag; filename |] ->
            resolveFilename filename
            |> lines
            |> Seq.toList
            |> resolveProcessor flag
            |> Seq.sum
            |> printfn "%A"
        | _ -> failwith "Usage: ./prog _ <filename | ->"

        0
    with _ as e ->
        printfn "%A" e.Message
        1
