(*
  Analysis of the results of football matches.
*)

type FootballResult =
    { HomeTeam: string
      AwayTeam: string
      HomeGoals: int
      AwayGoals: int }

/// Helper function to create a result
let createResult (ht, hg) (at, ag) =
    { HomeTeam = ht
      AwayTeam = at
      HomeGoals = hg
      AwayGoals = ag }

let results =
    [ createResult ("Juventus", 1) ("Chelsea", 2)
      createResult ("Juventus", 1) ("Real Madrid", 3)
      createResult ("Real Madrid", 3) ("Chelsea", 1)
      createResult ("Real Madrid", 2) ("Juventus", 1)
      createResult ("Chelsea", 4) ("Juventus", 2)
      createResult ("Chelsea", 1) ("Real Madrid", 2) ]

results
|> List.filter (fun x -> x.AwayGoals > x.HomeGoals)
|> List.countBy (fun x -> x.AwayTeam)
|> List.sortByDescending snd
|> List.head
|> fst

let winner =
    results
    |> List.filter (fun x -> x.AwayGoals > x.HomeGoals)
    |> List.maxBy (fun x -> x.AwayGoals)

winner.AwayTeam
