// ************ Model *************** //
type Details = { Name: string; Description: string }

type Item = { Details: Details }

type RoomId = RoomId of string

type Exit =
    | PassableExit of Details * destination: RoomId
    | LockedExit of Details * key: Item * next: Exit
    | NoExit of Exit option

and Room =
    { Id: RoomId
      Details: Details
      Items: Details list
      Exits: Exits }

and Exits =
    { North: Exit
      South: Exit
      East: Exit
      West: Exit }

type Player =
    { Details: Details
      Location: RoomId
      Inventory: Item list }

type World =
    { Rooms: Map<RoomId, Room>
      Player: Player }


module FunctionalGame =
    // **************** Initial World *************** //
    let firstRoom =
        { Id = RoomId "first"
          Details =
            { Name = "First room"
              Description = "This is the first room" }
          Items = []
          Exits =
            { North = NoExit None
              South = NoExit None
              East = NoExit None
              West = NoExit None } }

    let key: Item =
        { Details =
            { Name = "A shiny key"
              Description = "This key looks like it could open a nearby door." } }

    let allRooms =
        [ { Id = RoomId "center"
            Details =
              { Name = "A central room"
                Description =
                  "You are standing in a central room with exits in all directions.  
                            A single brazier lights the room." }
            Items = []
            Exits =
              { North =
                  PassableExit(
                      { Name = "North"
                        Description = "You see a darkened passageway to the north." },
                      RoomId "north1"
                  )
                South =
                  PassableExit(
                      { Name = "South"
                        Description = "You see door to the south.  A waft of cold air hits your face." },
                      RoomId "south1"
                  )
                East =
                  LockedExit(
                      { Name = "East"
                        Description = "You see a locked door to the east." },
                      key,
                      PassableExit(
                          { Name = "West"
                            Description = "You see an open door to the east." },
                          RoomId "east1"
                      )
                  )
                West =
                  PassableExit(
                      { Name = "West2"
                        Description = "You see an interesting room to the west." },
                      RoomId "west1"
                  ) } }
          { Id = RoomId "north1"
            Details =
              { Name = "A dark room"
                Description =
                  "You are standing in a very dark room.  
                            You hear the faint sound of rats scurrying along the floor." }
            Items = []
            Exits =
              { North = NoExit None
                South =
                  PassableExit(
                      { Name = "North"
                        Description = "You see an dimly lit room to the south." },
                      RoomId "center"
                  )
                East = NoExit None
                West = NoExit None } }
          { Id = RoomId "south1"
            Details =
              { Name = "A cold room"
                Description =
                  "You are standing in a room that feels very cold.  
                            Your breath instantly turns into a white puff." }
            Items = []
            Exits =
              { North =
                  PassableExit(
                      { Name = "North"
                        Description = "You see an exit to the north. That room looks much warmer." },
                      RoomId "center"
                  )
                South = NoExit None
                East = NoExit None
                West = NoExit None } }
          { Id = RoomId "west1"
            Details =
              { Name = "A cozy room"
                Description =
                  "This room seems very cozy, as if someone had made a home here.  
                            Various personal belongings are strewn about." }
            Items = []
            Exits =
              { North = NoExit None
                South = NoExit None
                East =
                  PassableExit(
                      { Name = "Cents"
                        Description = "You see a doorway back to the lit room." },
                      RoomId "center"
                  )
                West = NoExit None } }
          { Id = RoomId "east1"
            Details =
              { Name = "An open meadow"
                Description =
                  "You are in an open meadow.  
                            The sun is bright and it takes some time for your eyes to adjust." }
            Items = []
            Exits =
              { North = NoExit None
                South = NoExit None
                East = NoExit None
                West =
                  PassableExit(
                      { Name = "Track"
                        Description =
                          "You see stone doorway to the west.  
                                            Why would you want to go back there?" },
                      RoomId "center"
                  ) } } ]

    let player =
        { Details =
            { Name = "Luke"
              Description = "Just your average adventurer." }
          Inventory = []
          Location = RoomId "center" }

    let gameWorld =
        { Rooms = allRooms |> Seq.map (fun room -> (room.Id, room)) |> Map.ofSeq
          Player = player }



    // ******************** Logic ******************** //

    // Custom Result
    // type Result<'TSuccess, 'TFailure> =
    //     | Success of 'TSuccess
    //     | Failure of 'TFailure

    let bind processFunc lastResult =
        match lastResult with
        | Ok s -> processFunc s
        | Error f -> Error f

    let (>>=) x f = bind f x

    let switch processFunc input = Ok(processFunc input)

    let getRoom world roomId =
        match world.Rooms.TryFind roomId with
        | Some room -> Ok room
        | None -> Error "Room does not exist!"

    let describeDetails details =
        sprintf "\n\n%s\n\n%s\n\n" details.Name details.Description

    let extractDetailsFromRoom (room: Room) = room.Details

    let describeCurrentRoom world =
        world.Player.Location
        |> getRoom world
        |> (bind (switch extractDetailsFromRoom) >> bind (switch describeDetails))

    // let north ({ North = northExit }: Exits) = northExit
    // let south ({ South = southExit }: Exits) = southExit
    // let east ({ East = eastExit }: Exits) = eastExit
    // let west ({ West = westExit }: Exits) = westExit

    // Alternative approach
    let north exits = exits.North
    let south exits = exits.South
    let east exits = exits.East
    let west exits = exits.West

    let getCurrentRoom world = world.Player.Location |> getRoom world

    let setCurrentRoom world room =
        { world with
            Player = { world.Player with Location = room.Id } }

    let getExit direction exits =
        match (direction exits) with
        | PassableExit(_, roomId) -> Ok roomId
        | LockedExit(_, _, _) -> Error "There is a locked door in that direction."
        | NoExit(_) -> Error "There is no room in that direction."

    let move direction world =
        world
        |> getCurrentRoom
        >>= switch (fun room -> room.Exits)
        >>= getExit direction
        >>= getRoom world
        >>= switch (setCurrentRoom world)

    let displayResult result =
        match result with
        | Ok s -> printf "%s" s
        | Error f -> printf "%s" f

    gameWorld
    |> move south
    |> bind (move north)
    |> bind (move east)
    |> bind describeCurrentRoom
    |> displayResult


    // ******** Concurrent Event Loop for the Game Engine ******** //

    type GameEvent =
        | UpdateState of (World -> Result<World, string>)
        | ResetState of World
        | EndGameLoop

    let applyUpdate updateFunc worldState =
        match updateFunc worldState with
        | Ok newState ->
            describeCurrentRoom newState |> displayResult
            newState
        | Error message ->
            printfn "\n\n%s\n" message
            worldState

    type GameEngine(initialState: World) =
        let gameLoop =
            MailboxProcessor.Start(fun inbox ->
                let rec innerLoop worldState =
                    async {
                        let! eventMsg = inbox.Receive()

                        match eventMsg with
                        | UpdateState updateFunc -> return! innerLoop (applyUpdate updateFunc worldState)
                        | ResetState newState -> return! innerLoop newState
                        | EndGameLoop -> return ()
                    }

                innerLoop initialState)

        member this.ApplyUpdate(updateFunc) = gameLoop.Post(UpdateState updateFunc)

        member this.ResetState(newState) = gameLoop.Post(ResetState newState)

        member this.Stop() = gameLoop.Post(EndGameLoop)

    let gameEngine = GameEngine(gameWorld)
    gameEngine.ApplyUpdate(move south)

    let rand = System.Random()

    let playerController =
        MailboxProcessor.Start(fun inbox ->
            let rec innerLoop state =
                async {
                    try
                        let! eventMsg = inbox.Receive(2000)

                        if eventMsg = "Stop" then
                            return ()
                    with :? System.TimeoutException ->
                        [ "north", north; "south", south; "east", east; "west", west ]
                        |> List.item (rand.Next 4)
                        |> fun (dir, dirFunc) ->
                            printfn "Wandering %s..." dir
                            dirFunc
                        |> move
                        |> gameEngine.ApplyUpdate

                        do! innerLoop state
                }

            innerLoop 0)

    gameEngine.ResetState(gameWorld)

    playerController.Post("Stop")


// ************** Command Parsing * Text-based Recursive Decent Parser ***************** //

module CommandParser =
    open FunctionalGame

    type Parser<'a> = Parser of (char list -> Result<'a * char list, string>)

    let runParser parser inputChars =
        let (Parser parserFunc) = parser
        parserFunc inputChars

    let expectChar expectedChar =
        let innerParser inputChars =
            match inputChars with
            | c :: remainingChars ->
                if c = expectedChar then
                    Ok(c, remainingChars)
                else
                    Error(sprintf "Expected '%c', got '%c'" expectedChar c)
            | [] -> Error(sprintf "Expected '%c', reached end of input" expectedChar)

        Parser innerParser

    let stringToCharList str = List.ofSeq str

    let orParse parser1 parser2 =
        let innerParser inputChars =
            match runParser parser1 inputChars with
            | Ok result -> Ok result
            | Error _ -> runParser parser2 inputChars

        Parser innerParser

    let (<|>) = orParse

    let choice parserList = List.reduce orParse parserList

    let anyCharOf validChars =
        validChars |> List.map expectChar |> choice

    let andParse parser1 parser2 =
        let innerParser inputChars =
            match runParser parser1 inputChars with
            | Error msg -> Error msg
            | Ok(c1, remaining1) ->
                match runParser parser2 remaining1 with
                | Error msg -> Error msg
                | Ok(c2, remaining2) -> Ok((c1, c2), remaining2)

        Parser innerParser

    let (.>>.) = andParse

    let mapParser mapFunc parser =
        let innerParser inputChars =
            match runParser parser inputChars with
            | Error msg -> Error msg
            | Ok(result, remaining) -> Ok(mapFunc result, remaining)

        Parser innerParser

    let applyParser funcAsParser paramAsParser =
        (funcAsParser .>>. paramAsParser) |> mapParser (fun (f, x) -> f x)

    let (<*>) = applyParser

    let returnAsParser result =
        let innerParser inputChars = Ok(result, inputChars)

        Parser innerParser

    let liftToParser2 funcToLift paramAsParser1 paramAsParser2 =
        returnAsParser funcToLift <*> paramAsParser1 <*> paramAsParser2

    let rec sequenceParsers parserList =
        let cons head rest = head :: rest
        let consAsParser = liftToParser2 cons

        match parserList with
        | [] -> returnAsParser []
        | parser :: remainingParsers -> consAsParser parser (sequenceParsers remainingParsers)

    let charListAsString chars = System.String(List.toArray chars)

    let expectString expectedString =
        expectedString
        |> stringToCharList
        |> List.map expectChar
        |> sequenceParsers
        |> mapParser charListAsString

    stringToCharList "take"
    |> runParser (expectString "lake" <|> expectString "take")
    |> printfn "%A"
