module MullItOver

open Utils

open System.Text.RegularExpressions

type Opcode =
    | Do
    | Dont
    | Mul of int * int

let findMuls memory =
    let matches =
        Regex(@"do\(\)|don't\(\)|mul\(([0-9]{1,3}),([0-9]{1,3})\)").Matches(memory)

    seq {
        for m in matches do
            if m.Value = "do()" then
                yield Do
            elif m.Value = "don't()" then
                yield Dont
            else
                let mulVals = List.tail [ for x in m.Groups -> x.Value ] |> List.map int
                yield Mul(mulVals[0], mulVals[1])
    }

let computeOnlyMuls (acc, enabled) op =
    match op with
    | Mul(x, y) -> (acc + (x * y), true)
    | _ -> (acc, true)

let computeResult (acc, enabled) op =
    match op with
    | Do -> (acc, true)
    | Dont -> (acc, false)
    | Mul(x, y) -> if enabled then (acc + (x * y), true) else (acc, false)

let Day3Solution =
    let memoryFile =
        readFile @"/home/bob/src/AdventOfCode2024/src/AdventOfCode/day3/day3.txt"

    let memory = memoryFile |> Seq.head

    let muls = findMuls memory

    let resultP1, _ = ((0, true), muls) ||> Seq.fold computeOnlyMuls
    let resultP2, _ = ((0, true), muls) ||> Seq.fold computeResult

    printfn "%A" resultP1
    printfn "%A" resultP2
