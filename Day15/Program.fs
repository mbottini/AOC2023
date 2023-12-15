open Prelude.Prelude
open Prelude.PogSeq

type Operation =
    | Add
    | Remove

type Lens = { label: string; focalLength: int }

type LensStep = { lens: Lens; operation: Operation }

type Container = { boxes: Map<int, list<Lens>> }

let hash (s: string) =
    let helper curr c = (curr + int c) * 17 % 256
    Seq.fold helper 0 s

let hashLine = split ',' >> Seq.map hash >> Seq.sum

let parseAddStep s =
    match split '=' s with
    | [| label; numStr |] ->
        Some
            { lens =
                { label = label
                  focalLength = int numStr }
              operation = Add }
    | _ -> None

let parseRemoveStep (s: string) =
    match s |> Seq.toList |> List.rev with
    | '-' :: rest ->
        Some
            { lens =
                { label = List.rev rest |> System.String.Concat
                  focalLength = 0 }
              operation = Remove }
    | _ -> None

let parseLensStep s =
    List.map ((|>) s) [ parseAddStep; parseRemoveStep ]
    |> List.collect Option.toList
    |> List.head

let listAdder lens xs =
    match List.tryFindIndex (fun l -> l.label = lens.label) xs with
    | Some idx -> List.updateAt idx lens xs
    | None -> List.append xs [ lens ]

let listRemover lens xs =
    match List.tryFindIndex (fun l -> l.label = lens.label) xs with
    | Some idx -> List.removeAt idx xs
    | None -> xs

let listChanger ls =
    match ls.operation with
    | Add -> listAdder ls.lens
    | Remove -> listRemover ls.lens

let optionize f x =
    match x with
    | Some v -> Some(f v)
    | None -> Some(f [])

let containerChanger ls =
    Map.change (hash ls.lens.label) (listChanger ls |> optionize)

let parseFileP2 strings =
    Seq.map (parseLensStep >> containerChanger) strings

let changeMap funcs = Seq.fold (|>) Map.empty funcs

let totalPower boxNum slotNum lens =
    (1 + boxNum) * slotNum * lens.focalLength

let totalPowerPair k (v: list<Lens>) =
    Seq.map (uncurry (totalPower k)) (enumerate 1 v) |> Seq.sum

let totalPowerMap (m: Map<int, list<Lens>>) =
    Map.toSeq m |> Seq.map (uncurry totalPowerPair) |> Seq.sum

let processP2 line =
    split ',' line |> parseFileP2 |> changeMap |> totalPowerMap


[<EntryPoint>]
let main args =
    match args with
    | [| "-p1"; filename |] -> slurpOrStdin filename |> Seq.head |> hashLine |> printfn "%d"
    | [| "-p2"; filename |] -> slurpOrStdin filename |> Seq.head |> processP2 |> printfn "%d"
    | _ -> printfn "main error"

    0
