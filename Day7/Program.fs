type HandType =
    | HighCard
    | OnePair
    | TwoPair
    | Trips
    | FullHouse
    | Quads
    | Quints

type Card =
    | Joker
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace

let parseCard (c) =
    match c with
    | '2' -> Two
    | '3' -> Three
    | '4' -> Four
    | '5' -> Five
    | '6' -> Six
    | '7' -> Seven
    | '8' -> Eight
    | '9' -> Nine
    | 'T' -> Ten
    | 'J' -> Jack
    | 'Q' -> Queen
    | 'K' -> King
    | 'A' -> Ace
    | _ -> failwith "parse error"

let parseCard2 (c) =
    match c with
    | '2' -> Two
    | '3' -> Three
    | '4' -> Four
    | '5' -> Five
    | '6' -> Six
    | '7' -> Seven
    | '8' -> Eight
    | '9' -> Nine
    | 'T' -> Ten
    | 'J' -> Joker
    | 'Q' -> Queen
    | 'K' -> King
    | 'A' -> Ace
    | _ -> failwith "parse error"

let resolveCard (c: Card) : list<Card> =
    match c with
    | Joker -> [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Queen; King; Ace ]
    | c -> [ c ]

type Hand =
    { handType: HandType
      cards: list<Card> }

type HandBid = { hand: Hand; bid: int }

let frequencies sq =
    Seq.fold
        (fun acc k ->
            Map.change
                k
                (fun v ->
                    match v with
                    | None -> Some(1)
                    | Some(v) -> Some(v + 1))
                acc)
        Map.empty
        sq

let parseMultiples handType freq s =
    if frequencies s |> Map.exists (fun _ v -> v = freq) then
        Some { handType = handType; cards = s }
    else
        None

let parseQuints = parseMultiples Quints 5
let parseQuads = parseMultiples Quads 4

let parseTrips = parseMultiples Trips 3
let parseOnePair = parseMultiples OnePair 2

let parseFullHouse s : option<Hand> =
    if
        frequencies s
        |> fun m -> Map.exists (fun _ v -> v = 3) m && Map.exists (fun _ v -> v = 2) m
    then
        Some { handType = FullHouse; cards = s }
    else
        None

let parseTwoPair s =
    if (frequencies s |> Map.filter (fun _ v -> v = 2) |> Seq.length) = 2 then
        Some { handType = TwoPair; cards = s }
    else
        None

let parseHighCard s = Some { handType = HighCard; cards = s }

let handParsers =
    [ parseQuints
      parseQuads
      parseFullHouse
      parseTrips
      parseTwoPair
      parseOnePair
      parseHighCard ]

let rec parseHand' ps s =
    match ps with
    | p :: ps' ->
        match p s with
        | Some h -> h
        | None -> parseHand' ps' s
    | _ -> failwith "somehow out of parsers??"

let parseHand s = parseHand' handParsers s

let cons x xs = x :: xs

let rec product xss : list<list<'a>> =
    match xss with
    | [] -> [ [] ]
    | ys :: yss ->
        let prods = product yss

        seq {
            for y in ys do
                yield! List.map (cons y) prods
        }
        |> Seq.toList

let parseHand2 s =
    product (List.map resolveCard s) |> List.map parseHand |> List.max

let split (s: string) (delim: char) =
    s.Split(
        delim,
        System.StringSplitOptions.RemoveEmptyEntries
        ||| System.StringSplitOptions.TrimEntries
    )

let parseLine parser line =
    match split line ' ' with
    | [| handStr; bidStr |] ->
        { hand = parser handStr
          bid = int bidStr }
    | _ -> failwith "parse error"

let enumerate start xs =
    Seq.zip (Seq.initInfinite ((+) start)) xs

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
    | "-p1" -> Seq.map parseCard >> Seq.toList >> parseHand
    | "-p2" ->
        fun s ->
            let cs = Seq.map parseCard2 s

            Seq.map parseCard2 s
            |> Seq.toList
            |> parseHand2
            |> fun h -> { h with cards = cs |> Seq.toList }
    | _ -> failwith "flag error"

[<EntryPoint>]
let main args =
    try
        match args with
        | [| flag; filename |] ->
            resolveFilename filename
            |> lines
            |> Seq.map (parseLine (resolveParser flag))
            |> Seq.sort
            |> enumerate 1
            |> Seq.map (fun (idx, hand) -> idx * hand.bid)
            |> Seq.sum
            |> printfn "%d"

            0
        | _ ->
            printfn "Usage: ./prog -p<1|2> <filename>"
            1
    with _ as e ->
        printfn "%A" e.Message
        2
