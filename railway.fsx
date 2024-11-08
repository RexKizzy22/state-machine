module Railway

type Request = { name: string; email: string }

module RailwayCombinatorModule =
    // the two-track type
    type Result<'TSuccess, 'TFailure> =
        | Success of 'TSuccess
        | Failure of 'TFailure

    // convert a single value into a two-track result
    let succeed x = Success x

    // convert a single value into a two-track result
    let fail x = Failure x

    // apply either a success function or failure function
    let either successFunc failureFunc twoTrackInput =
        match twoTrackInput with
        | Success s -> successFunc s
        | Failure f -> failureFunc f

    // convert a switch function into a two-track function
    let bind f = either f fail

    // pipe a two-track value into a switch function
    let (>>=) x f = bind f x

    // compose two switches into another switch
    let (>=>) s1 s2 = s1 >> bind s2

    // convert a one-track function into a switch
    let switch f = f >> succeed

    // convert a one-track function into a two-track function
    let map f = either (f >> succeed) fail

    // convert a dead-end function into a one-track function
    let tee f x =
        f x
        x

    // convert a one-track function into a switch with exception handling
    let tryCatch f exnHandler x =
        try
            f x |> succeed
        with ex ->
            exnHandler ex |> fail

    // convert two one-track functions into a two-track function
    let doubleMap successFunc failureFunc =
        either (successFunc >> succeed) (failureFunc >> fail)

    // add two switches in parallel
    let plus addSuccess addFailure switch1 switch2 x =
        match (switch1 x), (switch2 x) with
        | Success s1, Success s2 -> Success(addSuccess s1 s2)
        | Failure f1, Success _ -> Failure f1
        | Success _, Failure f2 -> Failure f2
        | Failure f1, Failure f2 -> Failure(addFailure f1 f2)

module Combinator =

    open RailwayCombinatorModule

    let (&&&) v1 v2 =
        let addSuccess r1 r2 = r1 // return first
        let addFailure s1 s2 = s1 + "; " + s2 // concat
        plus addSuccess addFailure v1 v2

    let validate1 input =
        if input.name = "" then
            Failure "Name must not be blank"
        else
            Success input

    let validate2 input =
        if (input.name |> String.length) > 50 then
            Failure "Name must not be longer than 50 chars"
        else
            Success input

    let validate3 input =
        if input.email = "" then
            Failure "Email must not be blank"
        else
            Success input

    let log twoTrackInput =
        let success x =
            printfn $"DEBUG. Success so far: %A{x}"
            x

        let failure x =
            printfn $"ERROR. %A{x}"
            x

        doubleMap success failure twoTrackInput

    let combinedValidation = validate1 &&& validate2 &&& validate3

    let canonicalizeEmail input =
        { input with
            email = input.email.Trim().ToLower() }

    let updateDatabase input = () // dummy dead-end function for now

    // new function to handle exceptions
    let updateDatebaseStep = tryCatch (tee updateDatabase) (fun ex -> ex.Message)

    let usecase =
        combinedValidation >> map canonicalizeEmail >> bind updateDatebaseStep >> log
