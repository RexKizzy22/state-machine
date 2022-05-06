[<Measure>] type foot
[<Measure>] type inch

//conversion factor
let inchesPerFoot = 12.0<inch/foot>    

// test    
let distanceInFeet = 3.0<foot>    
let distanceInInches = distanceInFeet * inchesPerFoot

[<Measure>] type degC
[<Measure>] type degF

let convertDegCToF c = 
    c * 1.8<degF/degC> + 32.0<degF>

// test    
let f = convertDegCToF 0.0<degC>

let badConvertDegCToF c = 
    c * 1.8<degF/degC> + 32.0

module UnitOfMeasure =

    type Application = Application
    type Ids = Ids
    [<Measure>] type money
    type Deposit = decimal<money>
    type CheckingAccount = CheckingAccount

    let openCheckingAccount (deposit: Deposit) =
        if deposit > 500M<money> then
            Some CheckingAccount
        else None
