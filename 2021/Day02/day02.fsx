open System
open System.IO

type Axis =
    | X of int
    | Y of int

type Coordinate = { X: int; Y: int }

let strToAxis (str: string) =
    let [| directionStr; amountStr |] = str.Split(' ')
    let amount = int amountStr

    match directionStr.ToLower() with
    | "forward" -> X amount
    | "down" -> Y amount
    | "up" -> Y -amount

let readInput () =
    File.ReadLines("input02.txt")
    |> Seq.filter (fun x -> not (String.IsNullOrEmpty(x)))

let getPart1Result (readerCallback: unit -> string seq) =
    readerCallback ()
    |> Seq.map strToAxis
    |> Seq.fold
        (fun state curr ->
            match curr with
            | X x -> { state with X = state.X + x }
            | Y y -> { state with Y = state.Y + y })
        { X = 0; Y = 0 }
    |> (fun coord -> coord.X * coord.Y)

printfn "Day 02 part 01: %d" (getPart1Result readInput)


type Directive =
    | X of int
    | Aim of int

let strToDirective (str: string) =
    let [| directionStr; amountStr |] = str.Split(' ')
    let amount = int amountStr

    match directionStr.ToLower() with
    | "forward" -> X amount
    | "down" -> Aim amount
    | "up" -> Aim -amount

let getPart2Result (readerCallback: unit -> string seq) =
    readerCallback () 
    |> Seq.map strToDirective
    |> Seq.fold
        (fun (state: {| Aim: int; X: int; Y: int |}) curr ->
            match curr with
            | X x -> {| state with X = state.X + x; Y = state.Y + state.Aim * x |}
            | Aim a -> {| state with  Aim = state.Aim + a |})
        {| X = 0; Y = 0; Aim = 0 |}
    |> (fun coord -> coord.X * coord.Y)

printfn "Day 02 part 02: %d" (getPart2Result readInput)
