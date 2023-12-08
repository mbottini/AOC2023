namespace Prelude

module Prelude =
    let curry f = fun x y -> f (x, y)
    let curry3 f = fun x y z -> f (x, y, z)

    let flip f = fun x y -> f y x
    let ignore2 _ _ = ()

    let allMatches regex matcher s =
        (System.Text.RegularExpressions.Regex regex).Matches s |> Seq.map matcher
