[<Struct>]
type DiskType = HardDisk = 0 | MMC = 1 | SSD = 2

type FixedDisk =
    { DiskType : DiskType
      RPM : int option
      Platters : int option
      NumberOfPins : int option }

let seekRecord disk =
    match disk with
    | { DiskType = DiskType.HardDisk; RPM = Some 5400; Platters = Some 5; NumberOfPins = _; } -> 
        printfn "Seeking loudly and slowly!"
    | { DiskType = DiskType.HardDisk; RPM = _; Platters = Some 7; NumberOfPins = _ } -> 
        printfn "Seeking loudly but quickly!"
    | { DiskType = DiskType.MMC; RPM = _; Platters = _; NumberOfPins = _ } -> 
        printfn "Seeking quietly and slowly. This disk has %d pins!" disk.NumberOfPins.Value
    | { DiskType = DiskType.SSD; RPM = _; Platters = _; NumberOfPins = _ } -> 
        printfn "Already found it!"
    | _ -> printfn "Unknown disk"

type Disk =
| HardDisk of rpm:int * platters:int
| MMC of pins:int
| SSD

let seek disk = 
    match disk with
    | HardDisk (5400, 7) -> 
        printfn "Seeking loudly and slowly!"
    | MMC (pins) when pins = 5 -> 
        printfn "Seeking quietly and slowly. This disk has %d pins!" pins
    | SSD -> printfn "Seeking loudly but quickly!"
    | _ -> printfn "Already found it!"

let diskInstance = HardDisk (5400, 7)

seek diskInstance
 