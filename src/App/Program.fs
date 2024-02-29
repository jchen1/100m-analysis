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
      Reaction: decimal
      SplitData: decimal list
      Time: decimal }

type ParseState =
    { LastLine: LineType
      RaceData: RaceData list }

let cumulativeSplitDataRegex = Regex @"(?<LastName>[\p{L}-]+),? (?<FirstName>[\p{L}-]+) .*time(?<Splits>(?: [\d\.]+){10})"
let splitDataRegex = Regex @"reaction time (?<ReactionTime>[\d\.]+) interval(?: [\d\.]+){9}"
let parseRaceData (CumulativeSplitData line1) (SplitData line2) : Option<RaceData> =
    let match1 = cumulativeSplitDataRegex.Match line1
    let match2 = splitDataRegex.Match line2
    
    match match1.Success, match2.Success with
    | true, true ->
        let splits = match1.Groups.["Splits"].Value.Trim().Split(' ') |> Array.map decimal
        let reaction = decimal match2.Groups.["ReactionTime"].Value
        
        let splitData = (Seq.append (Seq.singleton reaction) splits) |> Seq.pairwise |> Seq.map (fun (a, b) -> b - a) |> Seq.toList
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
    
    // match state.LastLine, line with
    // | _, Ignored l when Regex.IsMatch(l, "reaction time [\d\.]+") -> printfn $"DID NOT PARSE: {l}"
    // | Ignored l, SplitData _ -> printfn $"DID NOT PARSE: {l}"
    // | _, _ -> ()
    
    { LastLine = line; RaceData = match raceData with Some data -> data :: state.RaceData | None -> state.RaceData } 

let lines = System.IO.File.ReadLines("data/M100m-by-meeting-Sep-23.txt")
let result = lines |> Seq.fold parseLine { LastLine = Ignored ""; RaceData = [] }

let printLine (r: RaceData) =
    let splits = String.concat ", " (r.SplitData |> List.map string)
    printfn $"{r.Athlete}, {r.Reaction}, {r.Time}, {splits}"
    
result.RaceData |> List.iter printLine
