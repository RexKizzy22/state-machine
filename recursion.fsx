// Factorial
// let rec fact n =
//     match n with
//     | 1 -> 1
//     | n -> n * fact (n-1)

// Tail call optimized
let fact n =
    let rec loop n acc =
        match n with
        | 1 -> acc
        | _ -> loop (n - 1) (acc * n)

    loop n 1


// Fibonacci sequence
// let rec fib (n:int64) =
//     match n with
//     | 0L -> 0L
//     | 1L -> 1L
//     | s -> fib (s-1L) + fib (s-2L)

// Tail call optimized
let fib (n: int64) =
    let rec loop n (a, b) =
        match n with
        | 0L -> a
        | 1L -> b
        | n -> loop (n - 1L) (b, a + b)

    loop n (0L, 1L)


// Fizzbuzz tail call optimized
let mapping = [ (3, "Fizz"); (5, "Buzz") ]

let fizzBuzz initialMapping n =
    let rec loop mapping acc =
        match mapping with
        | [] -> if acc = "" then string n else acc
        | head :: tail ->
            let value = head |> (fun (div, msg) -> if n % div = 0 then msg else "")
            loop tail (acc + value)

    loop initialMapping ""

[ 1..105 ] |> List.map (fizzBuzz mapping) |> List.iter (printfn "%s")


// Using List.fold
// let fizzBuzz n =
//     [ (3, "Fizz"); (5, "Buzz") ]
//     |> List.fold (fun acc (div, msg) ->
//         if n % div = 0 then acc + msg else acc) "" |> fun s -> if s = "" then string n else s

// [1..105]
// |> List.iter (fizzBuzz >> printfn "%s")

// Fizzbuzz with List.fold refactored
// let fizzBuzz n =
//     [ (3, "Fizz"); (5, "Buzz") ]
//     |> List.fold (fun acc (div, msg) ->
//         match (if n % div = 0 then msg else "") with
//         | "" -> acc
//         | s -> if acc = string n then s else acc + s) (string n)


// Quicksort algorithm
let rec qsort input =
    match input with
    | [] -> []
    | head :: tail ->
        let smaller, larger = List.partition (fun n -> head >= n) tail
        List.concat [ qsort smaller; [ head ]; qsort larger ]

[ 5; 9; 5; 2; 7; 9; 1; 1; 3; 5 ] |> qsort |> printfn "%A"



(*
    Episode 2.1 of the Trustbit Transport Tycoon challenge:
    https://github.com/trustbit/exercises/blob/master/transport-tycoon_21.md
*)

open System.IO

// type Tree<'a, 'b> =
//     | Node of 'a * tree<'a, 'b>
//     | Leaf of 'b

type Tree<'T> =
    | Branch of 'T * Tree<'T> seq
    | Leaf of 'T

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

// let run start finish =
//     Path.Combine(__SOURCE_DIRECTORY__, "resources", "data.csv") |> loadData
//     |> printfn "%A"
// let result = run "Cogburg" "Leverstorm"

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

// let findPossibleRoutes start finish (routeMap:Map<string, Connection list>) =
//     let rec loop current =
//         let nextRoutes = getUnvisited routeMap[current.Location] current
//         if nextRoutes |> List.isEmpty |> not && current.Location <> finish then
//             Branch (current, seq { for next in nextRoutes do loop next })
//         else
//             Leaf current
//     loop { Location = start; Route = []; TotalDistance = 0 }

// let run start finish =
//     Path.Combine(__SOURCE_DIRECTORY__, "resources", "data.csv") |> loadData
//     |> findPossibleRoutes start finish
//     |> printfn "%A"
// let result = run "Cogburg" "Leverstorm"

let rec treeToList tree =
    match tree with
    | Leaf x -> [ x ]
    | Branch(_, xs) -> List.collect treeToList (xs |> Seq.toList)

let findPossibleRoutes start finish (routeMap: Map<string, Connection list>) =
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

let run start finish =
    Path.Combine(__SOURCE_DIRECTORY__, "resources", "data.csv")
    |> loadData
    |> findPossibleRoutes start finish
    |> selectShortestRoute
    |> printfn "%A"

let result = run "Cogburg" "Leverstorm"
