module Tests
open NUnit.Framework
open FsUnit
open KataBowling

let createGameResults resultOfNormalTurn resultOfFinalTurn= 
    ([1..9] |> List.map (fun _ -> resultOfNormalTurn))
    @ [resultOfFinalTurn]
    
[<Test>]
let ``When the score of a perfect game is calculated the result should equal 300`` () =
    createGameResults Strike (Final ThreeStrikes) 
    |> calculateGameScore
    |> should equal 300    

[<Test>]
let ``When the score of a game with only Spare 5 is calculated the result should equal 150`` () =
    createGameResults (Spare 5) (Final (FinalTurnResult.Spare {FirstTry=5;ThirdTry=5}))
    |> calculateGameScore
    |> should equal 150

[<Test>]
let ``When the score of a game with only Points 9,0 is calculated the result should equal 90`` () =
    createGameResults (Points (9,0)) (Final (FinalTurnResult.Points (9,0)))
    |> calculateGameScore
    |> should equal 90    