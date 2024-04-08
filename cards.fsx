#r "nuget: NUnit"

module Cards =

    type Suit = 
        | Clubs
        | Diamonds
        | Hearts
        | Spades

    type Rank =
        | Two | Three | Four
        | Five | Six | Seven
        | Eight | Nine | Ten
        | Jack | Queen | King | Ace

    type Card = Card of Suit * Rank
    type Deck = Card list
    type ShuffledDeck = ShuffledDeck of Deck
    type Deal = ShuffledDeck -> Card option * ShuffledDeck 

    let ShuffledDeckSize (ShuffledDeck deck) = 
        List.length deck

    // making illegal state unrepresentable
    let createDeal (deck: ShuffledDeck) = 
        match deck with 
        | (ShuffledDeck (first::rest)) -> Some first, ShuffledDeck rest
        | (ShuffledDeck ([])) -> None, ShuffledDeck []

    let dealCard: Deal = createDeal

    // Fisher-Yates Shuffle Algorithm
    // let shuffle deck seed = 
    //     let rnd = System.Random(seed)
    //     let rec shuffler unshuffledDeck shuffledDeck =
    //         let count = unshuffledDeck |> List.length
    //         match count with
    //         | 0 -> ShuffledDeck shuffledDeck
    //         | _ -> 
    //             let index = rnd.Next(count)
    //             let randomCard = unshuffledDeck[index]
    //             let newUnshuffledDeck = unshuffledDeck |> List.filter (fun card -> deck <> randomCard)
    //             let newShuffledDeck = randomCard :: newUnshuffledDeck
    //             shuffler newUnShuffledDeck newShuffledDeck

    //     shuffler deck []

    // returns a deterministic output
    let shuffle deck seed = 
        let rnd = System.Random(seed)
        deck |> List.sortBy (fun _ -> rnd.Next()) |> ShuffledDeck 

    let allSuits = [Clubs; Diamonds; Hearts; Spades]
    let allRanks = [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace]

    let newDeck =
        [ for suit in allSuits do 
            for rank in allRanks do 
                Card (suit, rank) ] 

    printfn "%A" newDeck



module CardTest =

    open NUnit.Framework
    open Cards

    [<Test>]
    let ``should return 51 cards when dealing a card from a new deck``() =
        // Arrange
        let expected = 51
        let deck = shuffle newDeck 1

        // Act
        let _, rem = dealCard deck
        let actual = rem |> ShuffledDeckSize

        // Assert
        Assert.That(expected, Is.EqualTo actual, "There were not 51 cards in the deck, after dealing")

    [<Test>]
    let ``should return 0 cards when dealing with a card from a depleted deck``() = 
        // Arrange
        let expected = 0
        let deck = shuffle [] 1

        // Act
        let _, rem = dealCard deck
        let actual = rem |> ShuffledDeckSize

        // Assert
        Assert.That(expected, Is.EqualTo actual, "")

