open Prelude.Prelude
open Prelude.PogSeq

let nextSeq xs =
    Seq.windowed 2 xs
    |> Seq.map (fun arr -> (arr[0], arr[1]))
    |> Seq.map (uncurry (flip (-)))

let allSeqs xs =
    iterate nextSeq xs |> Seq.takeWhile (Seq.forall ((=) 0) >> not)

let processSeqs1 xss =
    Seq.map Seq.last xss |> Seq.rev |> Seq.scan (+) 0 |> Seq.last

let processSeqs2 xss =
    Seq.map Seq.head xss |> Seq.rev |> Seq.scan (flip (-)) 0 |> Seq.last

let parseFile ls = Seq.map (split ' ' >> Seq.map int) ls

let p1 xss =
    Seq.map allSeqs xss |> Seq.map processSeqs1 |> Seq.sum

let p2 xss =
    Seq.map allSeqs xss |> Seq.map processSeqs2 |> Seq.sum

let resolveProcessor s =
    match s with
    | "-p1" -> p1
    | "-p2" -> p2
    | _ -> failwith "flag error"

[<EntryPoint>]
let main args =
    match args with
    | [| flag; filename |] -> slurpOrStdin filename |> parseFile |> resolveProcessor flag |> printfn "%A"
    | _ -> printfn "%s" "main error"

    0
