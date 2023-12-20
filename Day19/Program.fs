open Prelude.Prelude
open Prelude.PogSeq

open FParsec

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
    member this.Cardinality = this.``end`` - this.start + 1L

type PartRange = Map<char, IntRange>

let prProduct (pr: PartRange) =
    Map.values pr |> Seq.map (_.Cardinality) |> Seq.fold (*) 1L

let prSum (pr: PartRange) =
    Map.values pr |> Seq.map (_.start) |> Seq.sum

let fullRange: PartRange =
    Seq.zip "xmas" (repeat { start = 1; ``end`` = 4000 }) |> Map

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
    { attribute: char
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

let parseAttr: Parser<char, string> = anyOf "xmas"
let parseComparison: Parser<Comparison, string> = anyOf "<>" |>> Comparison.Parse

let parseRule: Parser<Rule, string> =
    parse {
        let! attr = parseAttr
        let! cmp = parseComparison
        let! x = pint32
        let! res = pchar ':' >>. many1Chars asciiLetter |>> Result.Parse

        return
            ComparisonRule
                { attribute = attr
                  comparison = cmp
                  value = x
                  ifPass = res }
    }

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

let clampPartRange attr cmp v pr =
    let passRng, failRng = Map.find attr pr |> clampRange cmp v
    (Map.add attr passRng pr, Map.add attr failRng pr)

let validRange rng =
    match rng with
    | { start = start; ``end`` = ``end`` } when start <= ``end`` -> true
    | _ -> false

let validPartRange partRange =
    Map.forall (fun _ rng -> validRange rng) partRange

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

let parseSteps = sepBy1 parseStep (pchar ',') |>> applyMultipleRules

let parseWorkflow =
    tuple2 (many1Chars asciiLower) (between (pchar '{') (pchar '}') parseSteps)

let parseWorkflowLine s =
    match runParserOnString parseWorkflow "" "" s with
    | Success(f, _, _) -> f
    | _ -> failwith "parse error"

let parsePartLine s : PartRange =
    let vals = allMatches """\d+""" (fun m -> int64 m.Value |> IntRange.Singleton) s
    Seq.zip "xmas" vals |> Map

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
    runWorkflow m prs |> Seq.map (snd >> prSum) |> Seq.sum

let p2 m init =
    runWorkflow m init |> List.map (snd >> prProduct) |> List.sum

[<EntryPoint>]
let main args =
    match args with
    | [| "-p1"; filename |] -> slurpOrStdin filename |> parseFile |> uncurry p1 |> printfn "%d"
    | [| "-p2"; filename |] -> slurpOrStdin filename |> parseFile2 |> uncurry p2 |> printfn "%d"
    | _ -> printfn "main error"

    0
