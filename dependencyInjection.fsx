namespace Banking

module DependencyInjection =

    type Application = Application
    type Ids = Ids
    type Deposit = Deposit
    type CheckingAccount = CheckingAccount 

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