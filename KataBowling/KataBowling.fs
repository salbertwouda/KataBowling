module KataBowling

type FinalSpareResult = {FirstTry:int;ThirdTry:int}
type FinalTurnResult =
    | ThreeStrikes
    | TwoStrikes of int
    | Spare of FinalSpareResult
    | Points of int*int

type TurnResult = 
    | Strike
    | Spare of int
    | Points of int*int
    | Final of FinalTurnResult

let rec private getPointsOfAllTries = function
    | Strike -> 10
    | Spare _ -> 10
    | Points (pointsOfFirstTry,pointsOfSecondTry) -> pointsOfFirstTry + pointsOfSecondTry
    | Final result -> 
        match result with
        | ThreeStrikes -> 30
        | TwoStrikes p -> 20 + p
        | FinalTurnResult.Spare points -> 10 + points.ThirdTry
        | FinalTurnResult.Points (pointsOfFirstTry,pointsOfSecondtry) -> pointsOfFirstTry + pointsOfSecondtry

let rec private getPointsOfFirstTry = function
    | Strike -> 10
    | Spare pointsOfFirstTry -> pointsOfFirstTry
    | Points (pointsOfFirstTry,_) -> pointsOfFirstTry
    | Final result -> 
        match result with
        | ThreeStrikes
        | TwoStrikes _ -> 10
        | FinalTurnResult.Spare points -> points.FirstTry
        | FinalTurnResult.Points (pointsOfFirstTry,_) -> pointsOfFirstTry

let rec private collectBonuses results = 
    match results with
        | Strike::first::second::tail -> 
            let pointsOfSecondResult = second |> getPointsOfAllTries
            let rest = [first;second] @ tail |> collectBonuses 
            
            first
            |> getPointsOfAllTries
            |> (+) pointsOfSecondResult
            |> (+) rest
        | (Spare _)::first::tail -> 
            let rest = [first]@tail |> collectBonuses
            
            first
            |> getPointsOfFirstTry
            |> (+) rest
        | _::tail -> 
            (tail |> collectBonuses)
        | [] -> 0

let rec private collectScores results = 
    results
    |> List.map getPointsOfAllTries
    |> List.sum

let rec calculateGameScore results = 
    let bonus = results |> collectBonuses
    results 
    |> collectScores
    |> (+) bonus