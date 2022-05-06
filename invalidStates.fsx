namespace InvalidStates

// To open an account, a customer must provide two forms of identification

// Compiler does not check if two similar ids can open an account
module InvalidStates = 

    type CheckingAccount = CheckingAccount

    type Identification = 
        | DriverLicense of string
        | SSNCard of string
        | MilitaryId of string
        | Passport of string

    type TwoIds =
        | Ids of Identification * Identification

    let openCheckingAccount (ids: TwoIds) =

        let Ids (id1, id2) = ids
        // check whether id1 and id2 are valid
        CheckingAccount 

module InvalidStatesTest =

    open InvalidStates

    let goodIds = Ids (DriverLicense "abc", Passport "123") 
    let badIds = Ids (DriverLicense "abc", DriverLicense "123") 

    let account1 = openCheckingAccount goodIds
    let account2 = openCheckingAccount badIds


// Invalid states are made unrepresentable
module ValidStates =

    type CheckingAccount = CheckingAccount

    type Identification = 
        | DriverLicense of string
        | SSNCard of string
        | MilitaryId of string
        | Passport of string

    type TwoIds =
        private
        | Ids of Identification * Identification

    let openCheckingAccount (ids: TwoIds) =

        let Ids (id1, id2) = ids
        // check whether id1 and id2 are valid
        CheckingAccount 

    let makeTwoIds (id1: Identification) (id2: Identification) =
        match (id1,id2) with
        | (DriverLicense _) , (DriverLicense _) 
        | (SSNCard _), (SSNCard _) 
        | (MilitaryId _), (MilitaryId _)
        | (Passport _), (Passport _) -> None  
        | _ -> (id1,id2) |> Ids |> Some


module ValidStatesTest = 

    open ValidStates

    let goodIds = makeTwoIds (DriverLicense "abc") (Passport "123") 
    let account1 = openCheckingAccount goodIds.Value

    let badIds = makeTwoIds (DriverLicense "abc") (DriverLicense "123")
    let account2 = openCheckingAccount badIds.Value



