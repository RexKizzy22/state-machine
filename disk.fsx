namespace Disk

(*
    If the DiskType is HardDisk, has 5400 RPM and 5 Platters, return "Seeking loudly and slowly!"
    If the DiskType is HardDisk and has 7 Platters, return "Seeking loudly but quickly!"
    If the DiskType is MMC, return "Seeking quietly and slowly. This disk has %d pins!"
    If the disk is an SSD, return "Already found it!"
    Otherwise, return "Unknown disk".
*)


// Modelling a Disk using a reference type Enum
// type DiskType = HardDisk = 0 | MMC = 1 | SSD = 2

// Modelling a Disk using a value type Enum
[<Struct>]
type DiskType = HardDisk = 0 | MMC = 1 | SSD = 2

// Modelling a Disk using a Discriminated Union
type Disk =
        | HardDisk of rpm:int * platters:int
        | MMC of pins:int
        | SSD


module DiskOption =

    type FixedDisk =
        { 
            DiskType : DiskType
            RPM : int option
            Platters : int option
            NumberOfPins : int option 
        }

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


module DiskUnion = 
    type FixedDisk =
        { 
            DiskType : DiskType
            RPM : int
            Platters : int
            NumberOfPins : int 
        }

    let fixedDisk =
        { 
            DiskType = DiskType.MMC
            RPM = 30
            Platters = 45
            NumberOfPins = 23 
        }

    let seek (disk:DiskType, rpm:int, platters:int, numberOfPins:int) =
        match disk, rpm, platters, numberOfPins with
        | DiskType.HardDisk, 5400, _, 5 -> 
            printfn "Seeking loudly and slowly!"
        | DiskType.HardDisk, _, 7, _ -> 
            printfn "Seeking loudly but quickly!"
        | DiskType.MMC, _, _, _ -> 
            printfn "Seeking quietly and slowly. This disk has %d pins!" numberOfPins
        | DiskType.SSD, _, _, _ -> printfn "Already found it!"
        | _ -> printfn "Unknown disk"

    let seekRecord disk =
        match disk with
        | { DiskType = DiskType.HardDisk; RPM = 5400; Platters = _; NumberOfPins = 5; } -> 
            printfn "Seeking loudly and slowly!"
        | { DiskType = DiskType.HardDisk; RPM = _; Platters = 7; NumberOfPins = 23 } -> 
            printfn "Seeking loudly but quickly!"
        | { DiskType = DiskType.MMC; RPM = _; Platters = _; NumberOfPins = _ } -> 
            printfn "Seeking quietly and slowly. This disk has %d pins!" disk.NumberOfPins
        | { DiskType = DiskType.SSD; RPM = _; Platters = _; NumberOfPins = _ } -> 
            printfn "Already found it!"
        | _ -> printfn "Unknown disk"

    seekRecord fixedDisk
        