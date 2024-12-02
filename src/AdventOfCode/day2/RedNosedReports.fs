module RedNosedReports

open Utils

type SafeState =
    | Unsafe = 0
    | Safe = 1

type Direction =
    | Increasing = 0
    | Decreasing = 1
    | Fail = 2

type ReportState =
    { lastNumber: int
      direction: Direction
      safeState: SafeState }

let checkNext state lastNumber reportValue =
    let direction =
        if lastNumber < reportValue then Direction.Increasing
        elif lastNumber > reportValue then Direction.Decreasing
        else Direction.Fail

    let delta = abs (reportValue - lastNumber)
    let deltaBounded = delta > 0 && delta <= 3

    if
        direction = state.direction
        && deltaBounded
        && state.safeState = SafeState.Safe
        && state.direction <> Direction.Fail
    then
        SafeState.Safe
    else
        SafeState.Unsafe

let checkNextNumber state reportValue =
    let safeState = checkNext state state.lastNumber reportValue

    { lastNumber = reportValue
      direction = state.direction
      safeState = safeState }

let filterReport (report: array<int>) (index: int) =
    seq {
        for i in 0 .. report.Length - 1 do
            if i <> index then
                yield report[i]
    }
    |> Seq.toArray

let checkState (report: array<int>) =
    let direction =
        if report[0] < report[1] then Direction.Increasing
        elif report[0] > report[1] then Direction.Decreasing
        else Direction.Fail

    let state =
        { lastNumber = report[0]
          direction = direction
          safeState = SafeState.Safe }

    let state = (state, report[1..]) ||> Array.fold checkNextNumber
    state.safeState

let checkSafe (report: array<int>) =
    let state = checkState report

    let filteredReports =
        [ 0 .. report.Length - 1 ] |> List.map (report |> filterReport)

    let filteredState = filteredReports |> List.map checkState

    let finalState =
        (state, filteredState)
        ||> List.fold (fun current indexed ->
            match indexed with
            | SafeState.Safe -> SafeState.Safe
            | SafeState.Unsafe -> current
            | _ -> current)

    finalState

let Day2Solution =
    let reports =
        readFile @"/home/bob/src/AdventOfCode2024/src/AdventOfCode/day2/day2.txt"

    let intReports = reports |> Seq.map (fun line -> line.Split ' ' |> Array.map int)

    let safeState = intReports |> Seq.map checkSafe

    let safeSum =
        (0, safeState)
        ||> Seq.fold (fun acc state ->
            match state with
            | SafeState.Safe -> acc + 1
            | SafeState.Unsafe -> acc
            | _ -> acc)

    printfn "%A" safeSum
