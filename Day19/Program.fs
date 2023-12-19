open Prelude.Prelude
open Prelude.PogSeq

open FParsec

type Attribute =
    | X
    | M
    | A
    | S

    static member Parse c =
        match System.Char.ToLower c with
        | 'x' -> X
        | 'm' -> M
        | 'a' -> A
        | 's' -> S
        | _ -> failwith "parse error"

type Comparison =
    | LT
    | GT

    static member Parse c =
        match c with
        | '<' -> LT
        | '>' -> GT
        | _ -> failwith "parse error"

type IntRange =
    { start: int64
      ``end``: int64 }

    static member Singleton x = { start = x; ``end`` = x }

let cardinality { start = start; ``end`` = ``end`` } = ``end`` - start + 1L

type PartRange =
    { xRng: IntRange
      mRng: IntRange
      aRng: IntRange
      sRng: IntRange }

    member this.Product() =
        List.map (cardinality >> int64) [ this.xRng; this.mRng; this.aRng; this.sRng ]
        |> Seq.fold (*) 1L

    member this.PartSum() =
        List.map (_.start) [ this.xRng; this.mRng; this.aRng; this.sRng ]
        |> Seq.fold (+) 0L
        |> int64

let fullRange =
    { xRng = { start = 1; ``end`` = 4000 }
      mRng = { start = 1; ``end`` = 4000 }
      aRng = { start = 1; ``end`` = 4000 }
      sRng = { start = 1; ``end`` = 4000 } }

type Result =
    | Continue of string
    | Done of bool

    static member Parse s =
        match s with
        | "A" -> Done true
        | "R" -> Done false
        | _ -> Continue s

let isDone res =
    match res with
    | Done _ -> true
    | _ -> false

let isRejected res =
    match res with
    | Done false -> true
    | _ -> false

type ComparisonRule =
    { attribute: Attribute
      comparison: Comparison
      value: int
      ifPass: Result }

type DefaultRule = { result: Result }

type Rule =
    | ComparisonRule of ComparisonRule
    | DefaultRule of DefaultRule

let comparisonFunc c =
    match c with
    | LT -> (<)
    | GT -> (>)

let parseAttr: Parser<Attribute, string> = anyOf "xmas" |>> Attribute.Parse
let parseComparison: Parser<Comparison, string> = anyOf "<>" |>> Comparison.Parse

let parseRule: Parser<Rule, string> =
    pipe4
        parseAttr
        parseComparison
        pint32
        (pchar ':' >>. many1Chars asciiLetter |>> Result.Parse)
        (fun attr comp x res ->
            ComparisonRule
                { attribute = attr
                  comparison = comp
                  value = x
                  ifPass = res })

let parseDefault: Parser<Rule, string> =
    many1Chars asciiLetter
    |>> Result.Parse
    |>> (fun res -> DefaultRule { result = res })

let parseStep = attempt parseRule <|> attempt parseDefault

let clampRange cmp v { start = start; ``end`` = ``end`` } =
    match cmp with
    | GT ->
        { start = max (v + 1L) start
          ``end`` = ``end`` },
        { start = start
          ``end`` = min v ``end`` }
    | LT ->
        { start = start
          ``end`` = min (v - 1L) ``end`` },
        { start = max v start
          ``end`` = ``end`` }

let clampPartRange attr cmp v partRange =
    match attr with
    | X ->
        let passRange, failRange = clampRange cmp v (partRange.xRng)
        { partRange with xRng = passRange }, { partRange with xRng = failRange }
    | M ->
        let passRange, failRange = clampRange cmp v (partRange.mRng)
        { partRange with mRng = passRange }, { partRange with mRng = failRange }
    | A ->
        let passRange, failRange = clampRange cmp v (partRange.aRng)
        { partRange with aRng = passRange }, { partRange with aRng = failRange }
    | S ->
        let passRange, failRange = clampRange cmp v (partRange.sRng)
        { partRange with sRng = passRange }, { partRange with sRng = failRange }

let validRange rng =
    match rng with
    | { start = start; ``end`` = ``end`` } when start <= ``end`` -> true
    | _ -> false

let validPartRange partRange =
    List.forall validRange [ partRange.xRng; partRange.mRng; partRange.aRng; partRange.sRng ]

let applyRuleRange rule rng =
    match rule with
    | DefaultRule { result = res } -> ((res, rng), None)
    | ComparisonRule { attribute = attr
                       comparison = cmp
                       value = v
                       ifPass = res } ->
        let passRange, failRange = clampPartRange attr cmp v rng
        ((res, passRange)), Some failRange

let applyMultipleRules rules partRange =
    let rec helper rs rng acc =
        match rs with
        | [] -> failwith "Out of rules. This should never happen!"
        | r :: rs' ->
            match applyRuleRange r rng with
            | pass, None -> pass :: acc
            | pass, Some fail when not (validPartRange fail) -> pass :: acc
            | pass, Some fail -> helper rs' fail (pass :: acc)

    helper rules partRange []

let parseSteps2 = sepBy1 parseStep (pchar ',') |>> applyMultipleRules

let parseWorkflow =
    tuple2 (many1Chars asciiLower) (between (pchar '{') (pchar '}') parseSteps2)

let parseWorkflowLine s =
    match runParserOnString parseWorkflow "" "" s with
    | Success(f, _, _) -> f
    | _ -> failwith "parse error"

let parsePartLine s =
    match allMatches """\d+""" (fun m -> int m.Value) s |> Seq.toArray with
    | [| x; m; a; s |] ->
        { xRng = IntRange.Singleton x
          mRng = IntRange.Singleton m
          aRng = IntRange.Singleton a
          sRng = IntRange.Singleton s }

    | _ -> failwith "parse error"

let parseFile ls =
    match groupby ((<>) "") ls |> Seq.filter fst |> Seq.map snd |> Seq.toArray with
    | [| workflows; parts |] ->
        let m = Seq.map parseWorkflowLine workflows |> Map
        let ps = Seq.map parsePartLine parts |> Seq.toList
        (m, Seq.zip (repeat (Continue "in")) ps |> Seq.toList)
    | _ -> failwith "parse error"

let parseFile2 ls =
    match groupby ((<>) "") ls |> Seq.filter fst |> Seq.map snd |> Seq.head with
    | workflows ->
        let m = Seq.map parseWorkflowLine workflows |> Map
        (m, [ Continue "in", fullRange ])

let runWorkflowPR (m: Map<string, (PartRange -> list<Result * PartRange>)>) res pr =
    match res with
    | Done _ -> [ res, pr ]
    | Continue label -> (Map.find label m) pr

let runWorkflowStep m tups =
    List.collect (uncurry (runWorkflowPR m)) tups
    |> List.filter (snd >> validPartRange)
    |> List.filter (fst >> isRejected >> not)

let runWorkflow m init =
    iterate (runWorkflowStep m) init
    |> Seq.filter (List.forall (fst >> isDone))
    |> Seq.head

let p1 m prs =
    runWorkflow m prs |> Seq.map (snd >> fun rng -> rng.PartSum()) |> Seq.sum

let p2 m init =
    runWorkflow m init |> List.map (snd >> fun rng -> rng.Product()) |> List.sum

[<EntryPoint>]
let main args =
    match args with
    | [| "-p1"; filename |] -> slurpOrStdin filename |> parseFile |> uncurry p1 |> printfn "%d"
    | [| "-p2"; filename |] -> slurpOrStdin filename |> parseFile2 |> uncurry p2 |> printfn "%d"
    | _ -> printfn "main error"

    0
