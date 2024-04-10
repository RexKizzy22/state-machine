namespace Banking

(*
    A modelling exercise demonstrating the functional programming approach to dependency injection.

    It also demonstrates how we can leverage the type system to make illegal states unrepresentable.
    
    The domain explored here is checking account opening in a bank.

    To open an account, a customer must provide two forms of identification

    Compiler does not check if two similar ids can open an account
*)


type Application = Application
    type Ids = Ids
    type Deposit = Deposit
    type CheckingAccount = CheckingAccount 

type Identification = 
        | DriverLicense of string
        | SSNCard of string
        | MilitaryId of string
        | Passport of string

    type TwoIds =
        | Ids of Identification * Identification

module DependencyInjection =

    
    let validateApplication (application: Application) =
        true 
        
    let validateIds (ids: Ids) =
        true 

    let openCheckingAccount validateApplication validateIds (application: Application) (ids: Ids) (deposit: Deposit) =
        // verify that application is complete
        // verify that ids are visual
        // verify that deposit amount is sufficient

        // if everything is okay, then return the checkingAccount
        if not (application |> validateApplication) then None 
        elif not (ids |> validateIds) then None 
        else Some CheckingAccount


module InvalidStatesTest =

    let openCheckingAccount (ids: TwoIds) =
        let Ids (id1, id2) = ids
        // check whether id1 and id2 are valid
        CheckingAccount 

    let goodIds = Ids (DriverLicense "abc", Passport "123") 
    let badIds = Ids (DriverLicense "abc", DriverLicense "123") 

    let account1 = openCheckingAccount goodIds
    let account2 = openCheckingAccount badIds


// Invalid states are made unrepresentable
module ValidStatesTest = 
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

    let goodIds = makeTwoIds (DriverLicense "abc") (Passport "123") 
    let account1 = openCheckingAccount goodIds.Value

    let badIds = makeTwoIds (DriverLicense "abc") (DriverLicense "123")
    let account2 = openCheckingAccount badIds.Value


module CheckingAccount =

    // The compiler will be unable to detect if arguments are not passed in the correct order.
    // This is prone to logical bugs as arguments could be wrongfully positioned

    // type CheckingAccountApplication =
    //     {
    //         FirstName: string
    //         LastName: string
    //         PhoneNumber: string
    //         EmailAddress: string
    //     }

    // let makeCheckingAccountApplication firstName lastName phoneNumber emailAddress =
    //     {
    //         FirstName = firstName
    //         LastName = lastName
    //         PhoneNumber = phoneNumber
    //         EmailAddress = emailAddress
    //     } 

    // let application = makeCheckingAccountApplication "Henry" "Segwit" "111-222-333" "a@b.com"

    // A BETTER APPROACH 

    type FirstName = FirstName of string
    type LastName = LastName of string
    type PhoneNumber = PhoneNumber of string
    type EmailAddress = EmailAddress of string

    type CheckingAccountApplication =
        {
            FirstName: FirstName
            LastName: LastName
            HomePhone: PhoneNumber
            EmailAddress: EmailAddress
        }

    let makeCheckingAccountApplication firstName lastName phoneNumber emailAddress =
        {
            FirstName = firstName
            LastName = lastName
            HomePhone = phoneNumber
            EmailAddress = emailAddress
        } 

    let firstName = FirstName "Henry"
    let lastname = LastName "Segwit"
    let homePhone = PhoneNumber "111-222-333"
    let emailAddress = EmailAddress "a@b.com"

    let application = makeCheckingAccountApplication firstName lastname homePhone emailAddress


module CustomerDiscount = 
    type CustomerId = CustomerId of string
    type RegisteredCustomer = { Id : CustomerId }
    type UnregisteredCustomer = { Id : CustomerId }
    type ValidationError =
        | InputOutOfRange of string
        
    type Spend = private Spend of decimal 

    module Spend =
        let value input = input |> fun (Spend value) -> value
        let create input =
            if input >= 0.0M && input <= 1000.0M then
                Ok (Spend input) 
            else
                Error (InputOutOfRange "You can only spend between 0 and 1000") 
            
    type Total = decimal
    type DiscountPercentage = decimal
    type Customer =
        | Eligible of RegisteredCustomer 
        | Registered of RegisteredCustomer 
        | Guest of UnregisteredCustomer

    module Customer =
        let calculateDiscountPercentage spend customer : DiscountPercentage = 
            match customer with
            | Eligible _ -> if Spend.value spend >= 100.0M then 0.1M else 0.0M 
            | _ -> 0.0M

        let calculateTotal customer spend : Total = 
            customer
                |> calculateDiscountPercentage spend
                |> fun discountPercentage -> Spend.value spend * (1.0M - discountPercentage)

    let john = Eligible { Id = CustomerId "John" }
    let mary = Eligible { Id = CustomerId "Mary" }
    let richard = Registered { Id = CustomerId "Richard" } 
    let sarah = Guest { Id = CustomerId "Sarah" }
    let isEqualTo expected actual = expected = actual
    let assertEqual customer spent expected = 
        Spend.create spent
            |> Result.map (fun spend -> Customer.calculateTotal customer spend) 
            |> isEqualTo (Ok expected)

    let assertJohn = assertEqual john 100.0M 90.0M
    let assertMary = assertEqual mary 99.0M 99.0M
    let assertRichard = assertEqual richard 100.0M 100.0M 
    let assertSarah = assertEqual sarah 100.0M 100.0M




// ******** Units of Measure ********

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


module Dimension = 

    [<Measure>] type foot
    [<Measure>] type inch

    //conversion factor
    let inchesPerFoot = 12.0<inch/foot>    

    // test    
    let distanceInFeet = 3.0<foot>    
    let distanceInInches = distanceInFeet * inchesPerFoot

module Temperature = 

    [<Measure>] type degC
    [<Measure>] type degF

    let convertDegCToF c =  c * 1.8<degF/degC> + 32.0<degF>

    // test    
    let f = convertDegCToF 0.0<degC>

    let badConvertDegCToF c = 
        c * 1.8<degF/degC> + 32.0

