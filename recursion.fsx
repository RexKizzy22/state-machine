(*
    A showcase of recursion in F# using the following problems:
    - Factorial
    - Fibonacci
    - Fizzbuzz
    - Quicksort algorithm
    - The Trustbit Transport Tycoon challenge (https://github.com/trustbit/exercises/blob/master/transport-tycoon_21.md)
*)

// Factorial
let rec fact1 n =
    match n with
    | 1 -> 1
    | n -> n * fact1 (n - 1)

// Tail call optimized
let fact2 n =
    let rec loop n acc =
        match n with
        | 1 -> acc
        | _ -> loop (n - 1) (acc * n)

    loop n 1


// Fibonacci sequence
let rec fib1 (n: int64) =
    match n with
    | 0L -> 0L
    | 1L -> 1L
    | s -> fib1 (s - 1L) + fib1 (s - 2L)

// Tail call optimized
let fib2 (n: int64) =
    let rec loop n (a, b) =
        match n with
        | 0L -> a
        | 1L -> b
        | n -> loop (n - 1L) (b, a + b)

    loop n (0L, 1L)


// Fizzbuzz tail call optimized
let mapping = [ (3, "Fizz"); (5, "Buzz") ]

let fizzBuzz1 initialMapping n =
    let rec loop mapping acc =
        match mapping with
        | [] -> if acc = "" then string n else acc
        | head :: tail ->
            let value = head |> (fun (div, msg) -> if n % div = 0 then msg else "")
            loop tail (acc + value)

    loop initialMapping ""

[ 1..105 ] |> List.map (fizzBuzz1 mapping) |> List.iter (printfn "%s")


// Using List.fold
let fizzBuzz2 n =
    [ (3, "Fizz"); (5, "Buzz") ]
    |> List.fold (fun acc (div, msg) -> if n % div = 0 then acc + msg else acc) ""
    |> fun s -> if s = "" then string n else s

[ 1..105 ] |> List.iter (fizzBuzz2 >> printfn "%s")

// Fizzbuzz with List.fold refactored
let fizzBuzz3 n =
    [ (3, "Fizz"); (5, "Buzz") ]
    |> List.fold
        (fun acc (div, msg) ->
            match (if n % div = 0 then msg else "") with
            | "" -> acc
            | s -> if acc = string n then s else acc + s)
        (string n)


// Quick Sort algorithm
let rec qsort input =
    match input with
    | [] -> []
    | head :: tail ->
        let smaller, larger = List.partition (fun n -> head >= n) tail
        List.concat [ qsort smaller; [ head ]; qsort larger ]

[ 5; 9; 5; 2; 7; 9; 1; 1; 3; 5 ] |> qsort |> printfn "%A"

// let rec quicksort lst =
//     match lst with
//     | [] -> []
//     | pivot :: rest ->
//         let smallerOrEqual = List.filter (fun x -> x <= pivot) rest
//         let larger = List.filter (fun x -> x > pivot) rest
//         quicksort smallerOrEqual @ [ pivot ] @ quicksort larger

// Merge Sort Algorithm
let rec mergeSort lst =
    let rec merge left right =
        match left, right with
        | [], _ -> right
        | _, [] -> left
        | x :: xs, y :: ys -> if x < y then x :: merge xs right else y :: merge left ys

    match lst with
    | []
    | [ _ ] -> lst
    | _ ->
        let mid = List.length lst / 2
        let left = mergeSort (List.take mid lst)
        let right = mergeSort (List.skip mid lst)
        merge left right


// Binary Search
let rec binarySearch lst target =
    match lst with
    | [] -> false
    | _ ->
        let midIndex = List.length lst / 2
        let midValue = List.item midIndex lst

        if target = midValue then
            true
        elif target < midValue then
            binarySearch (List.take midIndex lst) target
        else
            binarySearch (List.skip (midIndex + 1) lst) target



(*
    Episode 2.1 of the Trustbit Transport Tycoon challenge:
    https://github.com/trustbit/exercises/blob/master/transport-tycoon_21.md
*)


// ************** Run Point 1 ****************

open System.IO

// type Tree<'a, 'b> =
//     | Node of 'a * tree<'a, 'b>
//     | Leaf of 'b

type Tree<'T> =
    | Branch of 'T * Tree<'T> seq
    | Leaf of 'T

// Holds data loaded from the csv file
type Connection =
    { Start: string
      Finish: string
      Distance: int }

let loadData path =
    path
    |> File.ReadLines
    |> Seq.skip 1
    |> fun rows ->
        [ for row in rows do
              match row.Split(",") with
              | [| start; finish; distance |] ->
                  { Start = start
                    Finish = finish
                    Distance = int distance }

                  { Start = finish
                    Finish = start
                    Distance = int distance }
              | _ -> failwith "Row is badly formed" ]
    |> List.groupBy (fun cn -> cn.Start)
    |> Map.ofList

let run1 start finish =
    Path.Combine(__SOURCE_DIRECTORY__, "resources", "data.csv")
    |> loadData
    |> printfn "%A"

let result1 = run1 "Cogburg" "Leverstorm"


// ************** Run Point 2 ****************


// represents information of where we are, how we got here and how much distance we have covered to get here
type Waypoint =
    { Location: string
      Route: string list
      TotalDistance: int }

let getUnvisited connections current =
    connections
    |> List.filter (fun cn -> current.Route |> List.exists (fun loc -> loc = cn.Finish) |> not)
    |> List.map (fun cn ->
        { Location = cn.Finish
          Route = cn.Start :: current.Route
          TotalDistance = cn.Distance + current.TotalDistance })

let findPossibleRoutes1 start finish (routeMap: Map<string, Connection list>) =
    let rec loop current =
        let nextRoutes = getUnvisited routeMap[current.Location] current

        if nextRoutes |> List.isEmpty |> not && current.Location <> finish then
            Branch(
                current,
                seq {
                    for next in nextRoutes do
                        loop next
                }
            )
        else
            Leaf current

    loop
        { Location = start
          Route = []
          TotalDistance = 0 }

let run2 start finish =
    Path.Combine(__SOURCE_DIRECTORY__, "resources", "data.csv")
    |> loadData
    |> findPossibleRoutes1 start finish
    |> printfn "%A"

let result2 = run2 "Cogburg" "Leverstorm"


// ************** Run Point 3 ****************


let rec treeToList tree =
    match tree with
    | Leaf x -> [ x ]
    | Branch(_, xs) -> List.collect treeToList (xs |> Seq.toList)

let findPossibleRoutes2 start finish (routeMap: Map<string, Connection list>) =
    let rec loop current =
        let nextRoutes = getUnvisited routeMap[current.Location] current

        if nextRoutes |> List.isEmpty |> not && current.Location <> finish then
            Branch(
                current,
                seq {
                    for next in nextRoutes do
                        loop next
                }
            )
        else
            Leaf current

    loop
        { Location = start
          Route = []
          TotalDistance = 0 }
    |> treeToList
    |> List.filter (fun wp -> wp.Location = finish)

let selectShortestRoute routes =
    routes
    |> List.minBy (fun wp -> wp.TotalDistance)
    |> fun wp -> wp.Location :: wp.Route |> List.rev, wp.TotalDistance

let run3 start finish =
    Path.Combine(__SOURCE_DIRECTORY__, "resources", "data.csv")
    |> loadData
    |> findPossibleRoutes2 start finish
    |> selectShortestRoute
    |> printfn "%A"

let result3 = run3 "Cogburg" "Leverstorm"
