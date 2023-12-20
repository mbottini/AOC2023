open Prelude.Prelude
open Prelude.PogSeq

open FSharpx.Collections
open FParsec

type FlipFlop =
    { state: bool
      destinations: list<string> }

    member this.Toggle = { this with state = not this.state }

type Inverter =
    { sourceStates: Map<string, bool>
      destinations: list<string> }

    member this.Update k v =
        { this with
            sourceStates = Map.add k v this.sourceStates }

type Broadcaster = { destinations: list<string> }
type Button = { destinations: list<string> }

[<StructuredFormatDisplay("{DisplayText}")>]
type Pulse =
    { source: string
      destination: string
      voltage: bool }

    override this.ToString() =
        sprintf "%s -%s-> %s" this.source (if this.voltage then "high" else "low") this.destination

    member this.DisplayText = this.ToString()

let buttonPulse =
    { source = "button"
      destination = "broadcaster"
      voltage = false }

type Component =
    | FlipFlop of FlipFlop
    | Inverter of Inverter
    | Broadcaster of Broadcaster
    | Button of Button
    | NoOp

    member this.Destinations =
        match this with
        | FlipFlop { destinations = destinations } -> destinations
        | Inverter { destinations = destinations } -> destinations
        | Broadcaster { destinations = destinations } -> destinations
        | Button { destinations = destinations } -> destinations
        | NoOp -> []

    member this.GetPulseState =
        match this with
        | FlipFlop { state = state } -> state
        | Inverter { sourceStates = sourceStates } -> Map.values sourceStates |> Seq.forall id |> not
        | Broadcaster _ -> false
        | Button _ -> false
        | NoOp -> failwith "This should never happen!"

    member this.SendPulse source destination =
        { source = source
          destination = destination
          voltage = this.GetPulseState }

    member this.ProcessPulse(p: Pulse) =
        let newComp, sendPulses =
            match this with
            | FlipFlop ff when not p.voltage -> ff.Toggle |> FlipFlop, true
            | FlipFlop _ -> this, false
            | Inverter inv -> inv.Update p.source p.voltage |> Inverter, true
            | NoOp -> this, false
            | _ -> this, true

        newComp,
        if sendPulses then
            List.map (fun d -> newComp.SendPulse p.destination d) (this.Destinations)
        else
            []

type PulseQueue = Queue<Pulse>

let parseLabel: Parser<string, string> = many1Chars asciiLetter

let parseDestinations: Parser<list<string>, string> =
    sepBy1 parseLabel (skipString ", ")

let parseArrow = skipString " -> "

let parseBroadcaster: Parser<string * Component, string> =
    parse {
        let! destLabels = skipString "broadcaster" >>. parseArrow >>. parseDestinations
        return "broadcaster", Broadcaster { destinations = destLabels }
    }

let parseFlipFlop: Parser<string * Component, string> =
    parse {
        let! compName = pchar '%' >>. parseLabel
        let! destLabels = parseArrow >>. parseDestinations

        return
            compName,
            FlipFlop
                { state = false
                  destinations = destLabels }
    }

let parseInverter: Parser<string * Component, string> =
    parse {
        let! compName = pchar '&' >>. parseLabel
        let! destLabels = parseArrow >>. parseDestinations

        return
            compName,
            Inverter
                { sourceStates = Map.empty
                  destinations = destLabels }
    }

let parseComponent: Parser<string * Component, string> =
    List.map attempt [ parseFlipFlop; parseInverter; parseBroadcaster ] |> choice

let runParser p s =
    match runParserOnString p "" "" s with
    | Success(res, _, _) -> res
    | _ -> failwith "parse error"

let connectInverters map =
    seq {
        for label, v in Map.toSeq map do
            let v' =
                match v with
                | Inverter inv ->
                    let sources =
                        seq {
                            for k, v in Map.toSeq map do
                                if List.contains label (v.Destinations) then
                                    yield k
                        }

                    Inverter
                        { inv with
                            sourceStates = Seq.zip sources (repeat false) |> Map }
                | _ -> v

            yield label, v'
    }
    |> Map

let parseFile ls =
    Seq.map (runParser parseComponent) ls |> Map |> connectInverters

let nextStep (map: Map<string, Component>) queue =
    match Queue.tryUncons queue with
    | Some(p, queue') ->
        let comp', pulses =
            (Map.tryFind p.destination map |> Option.defaultValue NoOp).ProcessPulse p

        Some(p, (Map.add p.destination comp' map, List.fold (flip Queue.conj) queue' pulses))
    | None -> None

let endlessPushing map =
    let rec helper buttonCount m queue =
        match nextStep m queue with
        | Some(p, (m', q')) ->
            seq {
                yield (buttonCount, p)
                yield! helper buttonCount m' q'
            }
        | None -> helper (buttonCount + 1) m (Queue.conj buttonPulse queue)

    helper 0 map Queue.empty

let findButtonsForDestination m dest =
    endlessPushing m
    |> Seq.filter (snd >> fun p -> p.destination = dest && (not p.voltage))
    |> Seq.head
    |> fst

let lcm a b =
    (a * b) / System.Numerics.BigInteger.GreatestCommonDivisor(a, b)

let m = slurpOrStdin "Day20/input.txt" |> parseFile

let p1 m =
    endlessPushing m
    |> Seq.takeWhile (fst >> (>=) 1000)
    |> Seq.groupBy (snd >> _.voltage)
    |> Seq.map (snd >> Seq.length)
    |> Seq.fold (*) 1

let p2 (m: Map<string, Component>) =
    let labels =
        match
            Map.values m
            |> Seq.filter (fun c -> List.contains "rx" c.Destinations)
            |> Seq.head
        with
        | Inverter { sourceStates = sourceStates } -> Map.keys sourceStates |> Seq.toList
        | _ -> failwith "Something is very badly wrong!"

    List.map (findButtonsForDestination m >> System.Numerics.BigInteger) labels
    |> List.fold lcm (System.Numerics.BigInteger.One)

[<EntryPoint>]
let main args =
    match args with
    | [| "-p1"; filename |] -> slurpOrStdin filename |> parseFile |> p1 |> printfn "%d"
    | [| "-p2"; filename |] -> slurpOrStdin filename |> parseFile |> p2 |> _.ToString() |> printfn "%s"
    | _ -> printfn "main error"

    0
