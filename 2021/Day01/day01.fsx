open System
open System.IO

let readInput () =
    File.ReadLines("input01.txt")
    |> Seq.filter (fun x -> not (String.IsNullOrEmpty(x)))
    |> Seq.map int

let rec countIncreasings count last levels =
    match levels with
    | [] -> count
    | current :: tail ->
        let cnt = if current > last then count + 1 else count
        countIncreasings cnt current tail

let getPart1Result (readerCallback: unit -> int seq) =
    readerCallback ()
    |> List.ofSeq
    |> countIncreasings 0 Int32.MaxValue

printfn "Day 1 part 1: %d" (getPart1Result readInput)

let getPart2Result (readerCallback: unit -> int seq) =
    readerCallback ()
    |> Seq.windowed 3
    |> Seq.map Array.sum
    |> List.ofSeq
    |> countIncreasings 0 Int32.MaxValue

printfn "Day 1 part 2: %d" (getPart2Result readInput)