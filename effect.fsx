(*
    A demonstration of asynchronous code in F#. 
    It showcases the composition of effectful functions with anonymous functions,
    as well as with the Option, Async and AsyncResult computation expressions.
*)

let multiply x y = // int -> int -> int
    x * y

let divide x y = // int -> int -> int option
    if y = 0 then None else Some(x / y)

module EffectDemo =
    let calculate1 x y =
        divide x y
        |> fun result ->
            match result with
            | Some v -> multiply v x |> Some
            | None -> None
        |> fun result ->
            match result with
            | Some t -> divide t y
            | None -> None

    let calculate2 x y =
        divide x y
        |> Option.map (fun v -> multiply v x)
        |> Option.bind (fun t -> divide t y)


module OptionDemo =

    type OptionBuilder() =
        // Supports let!
        member _.Bind(x, f) = Option.bind f x
        // Supports return
        member _.Return(x) = Some x

        // Supports return!
        member _.ReturnFrom(x) = x

    // Computation Expression for Option
    // Usage will be option {...}
    let option = OptionBuilder()

    let calculate3 x y =
        option {
            let! v = divide x y
            let t = multiply v x
            let! r = divide t y
            return r
        }

    let calculate4 x y =
        option {
            let! v = divide x y
            let t = multiply v x
            return! divide t y
        }

    calculate4 8 0 |> printfn "calculate 8 0 = %A" // None
    calculate4 8 2 |> printfn "calculate 8 2 = %A" // Some 16


#r "nuget: FsToolkit.ErrorHandling"

open FsToolkit.ErrorHandling

module ResultDemo =
    type Customer =
        { Id: int
          IsVip: bool
          Credit: decimal }

    // Customer -> Result<(Customer * decimal),exn>
    let getPurchases customer =
        try
            // Imagine this function is fetching data from a Database
            let purchases =
                if customer.Id % 2 = 0 then
                    (customer, 120M)
                else
                    (customer, 80M)

            Ok purchases
        with ex ->
            Error ex

    // Customer * decimal -> Customer
    let tryPromoteToVip purchases =
        let customer, amount = purchases

        if amount > 100M then
            { customer with IsVip = true }
        else
            customer

    // Customer -> Result<Customer,exn>
    let increaseCreditIfVip customer =
        try
            // Imagine this function could cause an exception
            let increase = if customer.IsVip then 100M else 50M

            Ok
                { customer with
                    Credit = customer.Credit + increase }
        with ex ->
            Error ex

    // Customer -> Result<Customer,exn>
    let upgradeCustomer1 customer =
        customer
        |> getPurchases
        |> Result.map tryPromoteToVip
        |> Result.bind increaseCreditIfVip

    let upgradeCustomer2 customer =
        result {
            let! purchases = getPurchases customer
            let promoted = tryPromoteToVip purchases
            return! increaseCreditIfVip promoted
        }


module AsyncDemo =

    open System.IO

    type FileResult = { Name: string; Length: int }

    let getFileInformation path =
        async {
            let! bytes = File.ReadAllBytesAsync(path) |> Async.AwaitTask
            let fileName = Path.GetFileName(path)

            return
                { Name = fileName
                  Length = bytes.Length }
        }

    Path.Combine(__SOURCE_DIRECTORY__, "resources", "customers.csv")
    |> getFileInformation
    |> Async.RunSynchronously
    |> printfn "%A"


module AsyncResultDemo =

    open System

    type AuthError = | UserBannedOrSuspended

    type TokenError = BadThingHappened of string

    type LoginError =
        | InvalidUser
        | InvalidPwd
        | Unauthorized of AuthError
        | TokenErr of TokenError

    type AuthToken = AuthToken of Guid

    type UserStatus =
        | Active
        | Suspended
        | Banned

    type User =
        { Name: string
          Password: string
          Status: UserStatus }

    [<Literal>]
    let ValidPassword = "password"

    [<Literal>]
    let ValidUser = "isvalid"

    [<Literal>]
    let SuspendedUser = "issuspended"

    [<Literal>]
    let BannedUser = "isbanned"

    [<Literal>]
    let BadLuckUser = "hasbadluck"

    [<Literal>]
    let AuthErrorMessage = "Earth's core stopped spinning"

    let tryGetUser username =
        async {
            let user =
                { Name = username
                  Password = ValidPassword
                  Status = Active }

            return
                match username with
                | ValidUser -> Some user
                | SuspendedUser -> Some { user with Status = Suspended }
                | BannedUser -> Some { user with Status = Banned }
                | BadLuckUser -> Some user
                | _ -> None
        }

    let isPwdValid password user = password = user.Password

    let authorize user =
        async {
            return
                match user.Status with
                | Active -> Ok()
                | _ -> UserBannedOrSuspended |> Error
        }

    let createAuthToken user =
        try
            if user.Name = BadLuckUser then
                failwith AuthErrorMessage
            else
                Guid.NewGuid() |> AuthToken |> Ok
        with ex ->
            ex.Message |> BadThingHappened |> Error

    let login username password : Async<Result<AuthToken, LoginError>> =
        asyncResult {
            let! user = username |> tryGetUser |> AsyncResult.requireSome InvalidUser
            do! user |> isPwdValid password |> Result.requireTrue InvalidPwd
            do! user |> authorize |> AsyncResult.mapError Unauthorized
            return! user |> createAuthToken |> Result.mapError TokenErr
        }


module AsyncResultDemoTests =

    open AsyncResultDemo

    [<Literal>]
    let BadPassword = "notpassword"

    [<Literal>]
    let NotValidUser = "notvalid"

    let isOk (input: Result<_, _>) : bool =
        match input with
        | Ok _ -> true
        | _ -> false

    let matchError (error: LoginError) (input: Result<_, LoginError>) =
        match input with
        | Error ex -> ex = error
        | _ -> false

    let runWithValidPassword (username: string) =
        login username ValidPassword |> Async.RunSynchronously

    let success =
        let result = runWithValidPassword ValidUser
        result |> isOk

    let badPassword =
        let result = login ValidUser BadPassword |> Async.RunSynchronously
        result |> matchError InvalidPwd

    let invalidUser = runWithValidPassword NotValidUser |> matchError InvalidUser

    let isSuspended =
        runWithValidPassword SuspendedUser
        |> matchError (UserBannedOrSuspended |> Unauthorized)

    let isBanned =
        let result = runWithValidPassword BannedUser
        result |> matchError (UserBannedOrSuspended |> Unauthorized)

    let hasBadLuck =
        let result = runWithValidPassword BadLuckUser
        result |> matchError (AuthErrorMessage |> BadThingHappened |> TokenErr)

    printfn "Success: %b" success
    printfn "BadPassword: %b" badPassword
    printfn "InvalidUser: %b" invalidUser
    printfn "IsSuspended: %b" isSuspended
    printfn "IsBanned: %b" isBanned
    printfn "HasBadLuck: %b" hasBadLuck
