namespace SingleCaseUnions

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
