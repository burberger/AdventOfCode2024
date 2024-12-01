module day1

open Utils

type LocationMap = Map<string, int>

let parseInts (str: string) : int * int =
    let arr = str.Split "   "
    (arr[0] |> int, arr[1] |> int)

let markRepeat x =
    match x with
    | Some s -> Some(s + 1)
    | None -> None


let getSimilarityWeight (repeats: Map<int, int>, idx: int) : int =
    let weight = repeats.TryFind idx

    match weight with
    | Some w -> w
    | None -> 0

let day1solution =
    let locations = readFile @"/home/bob/src/AdventOfCode2024/src/AdventOfCode/day1.txt"

    let leftList, rightList = locations |> Seq.map parseInts |> Seq.toList |> List.unzip
    let leftList = leftList |> List.sort
    let rightList = rightList |> List.sort

    let distanceList =
        (leftList, rightList) ||> List.map2 (fun left right -> abs (left - right))

    let sum = distanceList |> List.sum

    let repeatCount =
        (Map.empty, rightList)
        ||> List.fold (fun map current ->
            if map.ContainsKey current then
                map.Change(current, markRepeat)
            else
                map.Add(current, 1))

    let similarityWeights =
        leftList |> List.map (fun x -> getSimilarityWeight (repeatCount, x))

    let weightedSum =
        (leftList, similarityWeights)
        ||> List.map2 (fun dist weight -> dist * weight)
        |> List.sum

    printfn "%A" sum
    printfn "%A" weightedSum
