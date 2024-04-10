module ActivePattern 

(* 
    Rules for the football prediction game:

        - 300 points for predicting the correct score (e.g. 2-3 vs 2-3)
        - 100 points for predicting the correct result (e.g. 2-3 vs 0-2)
        - 15 points per home goal & 20 points per away goal using the lower of the predicted and actual scores

    Tests:
        (0, 0) (0, 0) = 400 // 300 + 100 + (0 * 15) + (0 * 20)
        (3, 2) (3, 2) = 485 // 300 + 100 + (3 * 15) + (2 * 20)
        (5, 1) (4, 3) = 180 // 0 + 100 + (4 * 15) + (1 * 20)
        (2, 1) (0, 7) = 20 // 0 + 0 + (0 * 15) + (1 * 20)
        (2, 2) (3, 3) = 170 // 0 + 100 + (2 * 15) + (2 * 20)
*)

type Score = int * int

// Partial Active Pattern
let (|CorrectScore|_|) (expected:Score, actual:Score) =
    if expected = actual then Some () else None

// Multi-case Active Pattern
let (|Draw|HomeWin|AwayWin|) (score:Score) =
    match score with
    | (h, a) when h = a -> Draw
    | (h, a) when h > a -> HomeWin
    | _ -> AwayWin

// Partial Active Pattern
let (|CorrectResult|_|) (expected:Score, actual:Score) =
    match (expected, actual) with
    | (Draw, Draw) -> Some ()
    | (HomeWin, HomeWin) -> Some ()
    | (AwayWin, AwayWin) -> Some ()
    | _ -> None

// Partial Active Pattern

// let (|CorrectResult|_|) (expected:Score, actual:Score) =
//     match (expected, actual) with
//     | ((h, a), (h', a')) when h = a && h' = a' -> Some ()
//     | ((h, a), (h', a')) when h > a && h' > a' -> Some ()
//     | ((h, a), (h', a')) when h < a && h' < a' -> Some ()
//     | _ -> None

let goalsScore1 (expected:Score) (actual:Score) =
    let (h, a) = expected
    let (h', a') = actual
    let home = [ h; h' ] |> List.min
    let away = [ a; a' ] |> List.min
    (home * 15) + (away * 20)

let goalsScore2 (expected:Score) (actual:Score) =
    let home = [ fst expected; fst actual ] |> List.min
    let away = [ snd expected; snd actual ] |> List.min
    (home * 15) + (away * 20)

let calculatePoints (expected:Score) (actual:Score) =
    let pointsForCorrectScore = 
        match (expected, actual) with
        | CorrectScore -> 300
        | _ -> 0
    let pointsForCorrectResult =
        match (expected, actual) with
        | CorrectResult -> 100
        | _ -> 0
    let pointsForGoals = goalsScore2 expected actual
    pointsForCorrectScore + pointsForCorrectResult + pointsForGoals


let assertnoScoreDrawCorrect = 
    calculatePoints (0, 0) (0, 0) = 400
let assertHomeWinExactMatch = 
    calculatePoints (3, 2) (3, 2) = 485
let assertHomeWin = 
    calculatePoints (5, 1) (4, 3) = 180
let assertIncorrect = 
    calculatePoints (2, 1) (0, 7) = 20
let assertDraw = 
    calculatePoints (2, 2) (3, 3) = 170


// Refactoring

let resultScore (expected:Score) (actual:Score) =
    match (expected, actual) with
    | CorrectScore -> 400
    | CorrectResult -> 100
    | _ -> 0

let calculatePoints2 (expected:Score) (actual:Score) =
    let pointsForResult = resultScore expected actual
    let pointsForGoals = goalsScore2 expected actual
    pointsForResult + pointsForGoals
    
let calculatePoints3 (expected:Score) (actual:Score) =
    [ resultScore; goalsScore2 ]
    |> List.sumBy (fun f -> f expected actual)

let assertnoScoreDrawCorrect2 = 
    calculatePoints3 (0, 0) (0, 0) = 400
let assertHomeWinExactMatch2 = 
    calculatePoints3 (3, 2) (3, 2) = 485
let assertHomeWin2 = 
    calculatePoints3 (5, 1) (4, 3) = 180
let assertIncorrect2 = 
    calculatePoints3 (2, 1) (0, 7) = 20
let assertDraw2 = 
    calculatePoints3 (2, 2) (3, 3) = 170

