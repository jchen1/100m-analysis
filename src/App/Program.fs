open System.Text.RegularExpressions

type LineType =
    | Ignored of string
    | MeetingHeader of string
    | RaceHeader of string
    | DataHeader of string
    | CumulativeSplitData of string
    | SplitData of string
    | VelocityData of string

type RaceData =
    { Athlete: string
      Reaction: decimal option
      SplitData: decimal list
      Time: decimal }

type ParseState =
    { LastLine: LineType
      RaceData: RaceData list }

let cumulativeSplitDataRegex = Regex @"(?<LastName>[\p{L}- ]+),? (?<FirstName>[\p{L}- ]+) .*time(?<Splits>(?: [\d\.]+){10})"
let splitDataRegex = Regex @"reaction time (?<ReactionTime>[\d\.]+)?\s*interval(?: [\d\.]+){9}"
let parseRaceData (CumulativeSplitData line1) (SplitData line2) : Option<RaceData> =
    let match1 = cumulativeSplitDataRegex.Match line1
    let match2 = splitDataRegex.Match line2
    
    match match1.Success, match2.Success with
    | true, true ->
        let splits = match1.Groups.["Splits"].Value.Trim().Split(' ') |> Array.map decimal
        let reaction =
            match match2.Groups.["ReactionTime"].Value with
            | "" -> None
            | reaction -> Some (decimal reaction)
        
        let splitData = (Seq.append (Seq.singleton (Option.defaultValue 0m reaction)) splits) |> Seq.pairwise |> Seq.map (fun (a, b) -> b - a) |> Seq.toList
        let firstName = match1.Groups.["FirstName"].Value
        let lastName = match1.Groups.["LastName"].Value
        
        Some { Athlete = $"{firstName} {lastName}"
               Reaction = reaction
               SplitData = splitData
               Time = Array.last splits }
    | _ -> None


let parseLine (state: ParseState) (rawLine: string) =
    let line =
        match rawLine with
        | line when cumulativeSplitDataRegex.IsMatch line -> CumulativeSplitData line
        | line when splitDataRegex.IsMatch line -> SplitData line
        | line -> Ignored line
    let raceData =
        match state.LastLine, line with
        | CumulativeSplitData _, SplitData _ -> parseRaceData state.LastLine line
        | _ -> None
    
    { LastLine = line; RaceData = match raceData with Some data -> data :: state.RaceData | None -> state.RaceData } 

let lines = System.IO.File.ReadLines("data/M100m-by-meeting-Sep-23.txt")
let result = lines |> Seq.fold parseLine { LastLine = Ignored ""; RaceData = [] }

let toCsvLine (r: RaceData) =
    let splits = String.concat ", " (r.SplitData |> List.map string)
    $"{r.Athlete}, {Option.defaultValue System.String.Empty (Option.map (_.ToString()) r.Reaction)}, {r.Time}, {splits}"
    
let csvData = result.RaceData |> List.map toCsvLine |> String.concat "\n"
let csvHeader = "Athlete, Reaction, Time, 10m, 20m, 30m, 40m, 50m, 60m, 70m, 80m, 90m, 100m"
let csv = $"{csvHeader}\n{csvData}"

System.IO.File.WriteAllText("/Users/jeff/Documents/Projects/100m-analysis/data/parsed.csv", csv)

printfn $"Parsed {result.RaceData.Length} records. Wrote to data/parsed.csv."
