module KataBowling

type FinalTurnResult =
    | ThreeStrikes
    | TwoStrikes of int
    | Spare of int
    | Points of int*int

type TurnResult = 
    | Strike
    | Spare of int
    | Points of int*int
    | Final of FinalTurnResult

let rec private getPointsOfAllTries = function
    | Strike -> 10
    | Spare _ -> 10
    | Points (x,y) -> x+y
    | Final r -> 
        match r with
        | ThreeStrikes -> 30
        | TwoStrikes p -> 20 + p
        | FinalTurnResult.Spare p -> 10 + p
        | FinalTurnResult.Points (x,y) -> x + y

let rec private getPointsOfFirstTry = function
    | Strike -> 10
    | Spare p -> p
    | Points (p,_) -> p
    | Final r -> 
        match r with
        | ThreeStrikes
        | TwoStrikes _ -> 10
        | FinalTurnResult.Spare p -> p
        | FinalTurnResult.Points (x,_) -> x

let rec private collectBonuses results = 
    match results with
        | Strike::first::second::tail -> 
            let secondPoints = second |> getPointsOfAllTries
            let rest = [first;second] @ tail |> collectBonuses 
            
            first
            |> getPointsOfAllTries
            |> (+) secondPoints
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