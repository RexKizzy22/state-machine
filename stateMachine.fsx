namespace StateMachine 

(*
    Conditions where state machines are beneficial
    - You have a set of mutually exclusive states with transitions between them.
    - The transitions are triggered by external events.
    - The states are exhaustive. That is, there are no other choices and 
        you must always handle all cases.
    - Each state might have associated data that should not be accessible when 
        the system is in another state.
    - There are static business rules that apply to the states.
*)

module Automobile =
    // state machines define rules that control how states change.
    // Given a current state X, a state machine will define what states X can change to 
    // and what events can cause states changes to occur. 
    // Current state => Event => New State.

    // Examples of state transition rules: Automobile

    // Not running -> ignition -> idling
    // Idling -> put in drive -> moving
    // Idling -> turn off -> not running 
    // Moving -> turn left -> moving 
    // Moving -> turn right -> moving 
    // Moving -> Stop -> idling

    type AutomobileState = 
        | NotRunning
        | Idling
        | Moving

    type AutomobileEvent = 
        | PutInDrive
        | TurnOff
        | TurnRight
        | TurnLeft
        | Stop
        | Ignition

    // Clients can still call this function with invalid transitions that can 
    // cause it to raise an exception hence the use of a better approach below.
    
    // let stateMachine (state, event) = 
    //     match state, event with 
    //     | NotRunning, Ignition -> Idling
    //     | Idling, PutInDrive -> Moving 
    //     | Idling, TurnOff -> NotRunning
    //     | Moving, TurnRight -> Moving 
    //     | Moving, TurnLeft -> Moving 
    //     | Moving, Stop -> Idling  
    //     | _ -> failwith "Invalid state transition"

    let private stateTransitions event =
        match event with 
        | Ignition -> Idling
        | PutInDrive -> Moving  
        | TurnOff -> NotRunning 
        | TurnRight -> Moving 
        | TurnLeft -> Moving 
        | Stop -> Idling 

    let private getEventForState state = 
        match state with 
        | NotRunning -> [| Ignition |]
        | Idling -> [| PutInDrive; TurnOff |]
        | Moving -> [| Stop; TurnRight; TurnLeft |]

    type AllowedEvent = 
        { 
            EventInfo: AutomobileEvent
            RaiseEvent: unit -> EventResult
        }
    and EventResult = 
        {
            CurrentState: AutomobileState
            AllowedEvents: AllowedEvent array
        }

    let rec private stateMachine event =
        let newState = stateTransitions event
        let newEvents = getEventForState newState
        {
            CurrentState = newState
            AllowedEvents = 
                newEvents 
                |> Array.map (fun e -> 
                    // delay evaluation of recursive call
                    let f () = stateMachine e 
                    {
                        EventInfo = e 
                        RaiseEvent = f
                    })

        }

    let init() = stateMachine TurnOff 


module AutomobileTest =

    open Automobile 

    let result1 = init()
    let result2 = result1.AllowedEvents.[0].RaiseEvent()