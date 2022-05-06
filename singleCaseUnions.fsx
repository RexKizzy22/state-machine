namespace SingleCaseUnions

module SingleCaseUnions =

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
    
