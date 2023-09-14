// type FizzBuzz(mapping) = 
//         let calculate n =
//             mapping
//             |> List.map (fun (v, s) -> if n % v = 0 then s else "") 
//             |> List.reduce (+)
//             |> fun s -> if s = "" then string n else s

//         member _.Calculate(value) = calculate value

type IFizzBuzz =
    abstract member Calculate : int -> string

type FizzBuzz(mapping) = 
    let calculate n =
        mapping
        |> List.map (fun (v, s) -> if n % v = 0 then s else "") |> List.reduce (+)
        |> fun s -> if s <> "" then s else string n
    
    interface IFizzBuzz with
        member _.Calculate(value) = calculate value

// let doFizzBuzz =
//     let fizzBuzz = FizzBuzz([(3, "Fizz");(5, "Buzz")]) :> IFizzBuzz 
//     [ 1..15 ] |> List.map (fun n -> fizzBuzz.Calculate(n)) 

let doFizzBuzz =
    let fizzBuzz = FizzBuzz([(3, "Fizz");(5, "Buzz")])
    [1..15]
    |> List.map (fun n -> (fizzBuzz :> IFizzBuzz).Calculate(n))


type IRecentlyUsedList =
    abstract member IsEmpty : bool
    abstract member Size : int
    abstract member Capacity : int
    abstract member Clear : unit -> unit 
    abstract member Add : string -> unit 
    abstract member TryGet : int -> string option

type RecentlyUsedList(capacity:int) =
    let items = ResizeArray<string>(capacity)
    let add item =
        items.Remove item |> ignore
        if items.Count = items.Capacity then items.RemoveAt 0 
        items.Add item
    let get index =
        if index >= 0 && index < items.Count then Some items.[items.Count - index - 1] else None

    interface IRecentlyUsedList with 
        member _.IsEmpty = items.Count = 0 
        member _.Size = items.Count 
        member _.Capacity = items.Capacity 
        member _.Clear() = items.Clear() 
        member _.Add(item) = add item 
        member _.TryGet(index) = get index

let mrul = RecentlyUsedList(5) :> IRecentlyUsedList 
mrul.Capacity // Should be 5
mrul.Add "Test"
mrul.Size // Should be 1 mrul.Capacity // Should be 5
mrul.Add "Test2"
mrul.Add "Test3"
mrul.Add "Test4"
mrul.Add "Test"
mrul.Add "Test6"
mrul.Add "Test7"
mrul.Add "Test"
mrul.Size // Should be 5
mrul.Capacity // Should be 5
mrul.TryGet(0) = Some "Test" // Should return true 
mrul.TryGet(4) = Some "Test3" // Should return true


// Equality
open System

type Coordinate(latitude: float, longitude: float) = 
    member _.Latitude = latitude
    member _.Longitude = longitude

// let c1 = Coordinate(25.0, 11.98) 
// let c2 = Coordinate(25.0, 11.98) 
// let c3 = c1
// c1 = c2 // false
// c1 = c3 // true - reference the same instance


[<AllowNullLiteral>]
type GpsCoordinate(latitude: float, longitude: float) =
    let equals (other: GpsCoordinate) = 
        if isNull other then
            false 
        else
            latitude = other.Latitude
            && longitude = other.Longitude
    member _.Latitude = latitude 
    member _.Longitude = longitude
    override this.GetHashCode() =
        hash (this.Latitude, this.Longitude)
    override _.Equals(obj) = 
        match obj with
        | :? GpsCoordinate as other -> equals other 
        | _ -> false
    
    interface IEquatable<GpsCoordinate> with 
        member _.Equals(other: GpsCoordinate) =
            equals other
    static member op_Equality(this: GpsCoordinate, other: GpsCoordinate) = 
        this.Equals(other)

let c1 = GpsCoordinate(25.0, 11.98) 
let c2 = GpsCoordinate(25.0, 11.98) 
c1 = c2 // true
