namespace Prelude

module Prelude =
    let curry f = fun x y -> f (x, y)
    let curry3 f = fun x y z -> f (x, y, z)

    let uncurry f = fun (x, y) -> f x y

    let flip f = fun x y -> f y x
    let ignore2 _ _ = ()

    let split (delim: char) (s: string) =
        s.Split(
            delim,
            System.StringSplitOptions.RemoveEmptyEntries
            ||| System.StringSplitOptions.TrimEntries
        )

    let reMatch regex matcher s =
        (System.Text.RegularExpressions.Regex regex).Match s |> matcher

    let allMatches regex matcher s =
        (System.Text.RegularExpressions.Regex regex).Matches s |> Seq.map matcher

    let rec zip xs ys =
        match xs, ys with
        | x :: xs', y :: ys' ->
            seq {
                yield (x, y)
                yield! zip xs' ys'
            }
        | _ -> Seq.empty

    let rec zipLongest v xs ys =
        match xs, ys with
        | x :: xs', y :: ys' ->
            seq {
                yield (x, y)
                yield! zipLongest v xs' ys'
            }
        | [], y :: ys' ->
            seq {
                yield (v, y)
                yield! zipLongest v [] ys'
            }
        | x :: xs', [] ->
            seq {
                yield (x, v)
                yield! zipLongest v xs' []
            }
        | _ -> Seq.empty

    let pickOne xs =
        let rec helper acc xs =
            match xs with
            | [] -> Seq.empty
            | x :: xs' ->
                seq {
                    yield (x, List.rev acc @ xs')
                    yield! helper (x :: acc) xs'
                }

        helper [] xs
