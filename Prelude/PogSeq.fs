namespace Prelude

open Prelude

module PogSeq =
    let rec iterate f x =
        seq {
            yield x
            yield! iterate f (f x)
        }

    let rec repeatedly f =
        seq {
            yield f ()
            yield! repeatedly f
        }

    let product xs = Seq.fold (*) 1 xs

    let uncons xs =
        Seq.tryHead xs |> Option.map (fun x -> (x, Seq.tail xs))

    let intersperse x xs =
        match uncons xs with
        | Some(y, zs) ->
            seq {
                yield y

                for z in zs do
                    yield x
                    yield z
            }
        | None -> Seq.empty

    let intercalate xs yss = intersperse xs yss |> Seq.concat

    let force xs = Seq.fold ignore2 () xs

    let count n = iterate ((+) 1) n

    let enumerate n xs = Seq.zip (count n) xs

    let repeat x = repeatedly (fun () -> x)

    let cycle xs = repeat xs |> Seq.concat

    let lines (stream: System.IO.Stream) =
        let sr = new System.IO.StreamReader(stream)
        repeatedly sr.ReadLine |> Seq.takeWhile ((<>) null)

    let slurp = System.IO.File.OpenRead >> lines

    let slurpOrStdin path =
        match path with
        | "-" -> System.Console.OpenStandardInput 4096 |> lines
        | _ -> slurp path

    let takeWhile = Seq.takeWhile

    let dropWhile pred xs =
        let mutable flag = false

        seq {
            for x in xs do
                if not (pred x) then
                    flag <- true

                if flag then
                    yield x
        }

    let groupby f xs =
        let mutable curr = []
        let mutable v = None
        let returnF = f >> Some

        seq {
            for x in xs do
                let v' = returnF x

                if v' = v then
                    curr <- x :: curr
                else
                    if not (List.isEmpty curr) then
                        yield (v.Value, List.rev curr)

                    curr <- [ x ]
                    v <- v'

            if not (List.isEmpty curr) then
                yield (v.Value, List.rev curr)
        }

    let take n xs =
        enumerate 0 xs |> takeWhile (fun (idx, _) -> idx < n) |> Seq.map snd

    let drop = Seq.skip

    let foldl = Seq.fold

    let concat = Seq.concat

    let length = Seq.length

    let cons x xs =
        seq {
            yield x
            yield! xs
        }

    let peek f xs =
        seq {
            for x in xs do
                f x
                yield x
        }

    let frequencies xs =
        let increment x =
            Option.orElse (Some 0) x |> Option.map ((+) 1)

        Seq.fold (fun m x -> Map.change x increment m) Map.empty xs
