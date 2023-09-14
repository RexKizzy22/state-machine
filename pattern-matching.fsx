open System

// If the DiskType is HardDisk, has 5400 RPM and 5 Platters, return "Seeking loudly and slowly!"
// If the DiskType is HardDisk and has 7 Platters, return "Seeking loudly but quickly!"
// If the DiskType is MMC, return "Seeking quietly and slowly. This disk has %d pins!"
// If the disk is an SSD, return "Already found it!"
// Otherwise, return "Unknown disk".

type DiskType = HardDisk = 0 | MMC = 1 | SSD = 2

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

type FixedDisk =
    { DiskType : DiskType
      RPM : int
      Platters : int
      NumberOfPins : int }

let fixedDisk =
    { DiskType = DiskType.MMC
      RPM = 30
      Platters = 45
      NumberOfPins = 23 }

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