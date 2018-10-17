open System

type gameState = {
    Possession : bool
    CurrentDown : int
    YardLine : int
    YardsToFirstDown : int
    PlaysRemaining : int
    TeamOneScore : int
    TeamTwoScore : int
    }
    
let initState = {
    Possession = true
    CurrentDown = 1
    YardLine = 25
    YardsToFirstDown = 10
    PlaysRemaining = 120
    TeamOneScore = 0
    TeamTwoScore = 0
    }

let rand = Random()

let rec PlayGame gameState = 
    printfn "Current Game State:"
    if gameState.Possession
    then printfn "Possession: Team One"
    else printfn "Possession: Team Two"
    printfn "Current Down: %i" gameState.CurrentDown
    printfn "Yard Line: %i" gameState.YardLine
    printfn "Yards Until First Down: %i" gameState.YardsToFirstDown
    printfn "Number of plays remaining in game: %i" gameState.PlaysRemaining
    printfn "Team One Score: %i" gameState.TeamOneScore
    printfn "Team Two Score: %i" gameState.TeamTwoScore

    if gameState.Possession
    then
        printfn "Offense: Would you like to run or pass?"
        printfn "1. Run"
        printfn "2. Pass"
        let play = Console.ReadLine()
        if play = "1"
        then
            printfn "Executing run play"
            let yardGain = rand.Next(0, 15) - 5
            printfn "%i yards on the play!" yardGain
            if yardGain >= 100 - gameState.YardLine
            then
                let newState = {
                    Possession = false
                    CurrentDown = 1
                    YardLine = 25
                    YardsToFirstDown = 10
                    PlaysRemaining = gameState.PlaysRemaining - 1
                    TeamOneScore = gameState.TeamOneScore + 7
                    TeamTwoScore = gameState.TeamTwoScore
                }
                printfn ""
                PlayGame newState
            else if yardGain >= gameState.YardsToFirstDown
            then
                let newState = {
                    Possession = gameState.Possession
                    CurrentDown = 1
                    YardLine = gameState.YardLine + yardGain
                    YardsToFirstDown = 10
                    PlaysRemaining = gameState.PlaysRemaining - 1
                    TeamOneScore = gameState.TeamOneScore
                    TeamTwoScore = gameState.TeamTwoScore
                }
                printfn ""
                PlayGame newState
            else if gameState.CurrentDown = 4 && yardGain < gameState.YardsToFirstDown
            then
                let newPossession = false
                let newYardLine  = 100 - gameState.YardLine
                let newState = {
                    Possession = newPossession
                    CurrentDown = 1
                    YardLine = newYardLine
                    YardsToFirstDown = 10
                    PlaysRemaining = gameState.PlaysRemaining - 1
                    TeamOneScore = gameState.TeamOneScore
                    TeamTwoScore = gameState.TeamTwoScore
                }
                printfn ""
                PlayGame newState
            else
                let newState = {
                    Possession = gameState.Possession
                    CurrentDown = gameState.CurrentDown + 1
                    YardLine = gameState.YardLine + yardGain
                    YardsToFirstDown = gameState.YardsToFirstDown - yardGain
                    PlaysRemaining = gameState.PlaysRemaining - 1
                    TeamOneScore = gameState.TeamOneScore
                    TeamTwoScore = gameState.TeamTwoScore
                }
                printfn ""
                PlayGame newState
        else if play = "2"
        then
            printfn "Executing pass play"
            let yardGain = rand.Next(0, 55) - 15
            printfn "%i yards on the play!" yardGain
            if yardGain >= 100 - gameState.YardLine
            then
                let newState = {
                    Possession = false
                    CurrentDown = 1
                    YardLine = 25
                    YardsToFirstDown = 10
                    PlaysRemaining = gameState.PlaysRemaining - 1
                    TeamOneScore = gameState.TeamOneScore + 7
                    TeamTwoScore = gameState.TeamTwoScore
                }
                printfn ""
                PlayGame newState
            else if yardGain >= gameState.YardsToFirstDown
            then
                let newState = {
                    Possession = gameState.Possession
                    CurrentDown = 1
                    YardLine = gameState.YardLine + yardGain
                    YardsToFirstDown = 10
                    PlaysRemaining = gameState.PlaysRemaining - 1
                    TeamOneScore = gameState.TeamOneScore
                    TeamTwoScore = gameState.TeamTwoScore
                }
                printfn ""
                PlayGame newState
            else if gameState.CurrentDown = 4 && yardGain < gameState.YardsToFirstDown
            then
                let newPossession = false
                let newYardLine  = 100 - gameState.YardLine
                let newState = {
                    Possession = newPossession
                    CurrentDown = 1
                    YardLine = newYardLine
                    YardsToFirstDown = 10
                    PlaysRemaining = gameState.PlaysRemaining - 1
                    TeamOneScore = gameState.TeamOneScore
                    TeamTwoScore = gameState.TeamTwoScore
                }
                printfn ""
                PlayGame newState
            else
                let newState = {
                    Possession = gameState.Possession
                    CurrentDown = gameState.CurrentDown + 1
                    YardLine = gameState.YardLine + yardGain
                    YardsToFirstDown = gameState.YardsToFirstDown - yardGain
                    PlaysRemaining = gameState.PlaysRemaining - 1
                    TeamOneScore = gameState.TeamOneScore
                    TeamTwoScore = gameState.TeamTwoScore
                }
                printfn ""
                PlayGame newState
        else
            printfn "Invalid selection."
            printfn ""
            PlayGame gameState
    else
        printfn "Defense: Would you like to defend against run or pass?"
        printfn "1. Run"
        printfn "2. Pass"
        let play = Console.ReadLine()
        if play = "1"
        then
            printfn "Executing run defense"
            let yardGain = rand.Next(0, 55) - 15
            printfn "%i yards on the play!" yardGain
            if yardGain >= 100 - gameState.YardLine
            then
                let newState = {
                    Possession = true
                    CurrentDown = 1
                    YardLine = 25
                    YardsToFirstDown = 10
                    PlaysRemaining = gameState.PlaysRemaining - 1
                    TeamOneScore = gameState.TeamOneScore
                    TeamTwoScore = gameState.TeamTwoScore + 7
                }
                printfn ""
                PlayGame newState
            else if yardGain >= gameState.YardsToFirstDown
            then
                let newState = {
                    Possession = gameState.Possession
                    CurrentDown = 1
                    YardLine = gameState.YardLine + yardGain
                    YardsToFirstDown = 10
                    PlaysRemaining = gameState.PlaysRemaining - 1
                    TeamOneScore = gameState.TeamOneScore
                    TeamTwoScore = gameState.TeamTwoScore
                }
                printfn ""
                PlayGame newState
            else if gameState.CurrentDown = 4 && yardGain < gameState.YardsToFirstDown
            then
                let newPossession = true
                let newYardLine  = 100 - gameState.YardLine
                let newState = {
                    Possession = newPossession
                    CurrentDown = 1
                    YardLine = newYardLine
                    YardsToFirstDown = 10
                    PlaysRemaining = gameState.PlaysRemaining - 1
                    TeamOneScore = gameState.TeamOneScore
                    TeamTwoScore = gameState.TeamTwoScore
                }
                printfn ""
                PlayGame newState
            else
                let newState = {
                    Possession = gameState.Possession
                    CurrentDown = gameState.CurrentDown + 1
                    YardLine = gameState.YardLine + yardGain
                    YardsToFirstDown = gameState.YardsToFirstDown - yardGain
                    PlaysRemaining = gameState.PlaysRemaining - 1
                    TeamOneScore = gameState.TeamOneScore
                    TeamTwoScore = gameState.TeamTwoScore
                }
                printfn ""
                PlayGame newState
        else if play = "2"
        then
            printfn "Executing pass defense"
            let yardGain = rand.Next(0, 15) - 5
            printfn "%i yards on the play!" yardGain
            if yardGain >= 100 - gameState.YardLine
            then
                let newState = {
                    Possession = true
                    CurrentDown = 1
                    YardLine = 25
                    YardsToFirstDown = 10
                    PlaysRemaining = gameState.PlaysRemaining - 1
                    TeamOneScore = gameState.TeamOneScore 
                    TeamTwoScore = gameState.TeamTwoScore + 7
                }
                printfn ""
                PlayGame newState
            else if yardGain >= gameState.YardsToFirstDown
            then
                let newState = {
                    Possession = false
                    CurrentDown = 1
                    YardLine = gameState.YardLine + yardGain
                    YardsToFirstDown = 10
                    PlaysRemaining = gameState.PlaysRemaining - 1
                    TeamOneScore = gameState.TeamOneScore
                    TeamTwoScore = gameState.TeamTwoScore
                }
                printfn ""
                PlayGame newState
            else if gameState.CurrentDown = 4 && yardGain < gameState.YardsToFirstDown
            then
                let newPossession = true
                let newYardLine  = 100 - gameState.YardLine
                let newState = {
                    Possession = newPossession
                    CurrentDown = 1
                    YardLine = newYardLine
                    YardsToFirstDown = 10
                    PlaysRemaining = gameState.PlaysRemaining - 1
                    TeamOneScore = gameState.TeamOneScore
                    TeamTwoScore = gameState.TeamTwoScore
                }
                printfn ""
                PlayGame newState
            else
                let newState = {
                    Possession = gameState.Possession
                    CurrentDown = gameState.CurrentDown + 1
                    YardLine = gameState.YardLine + yardGain
                    YardsToFirstDown = gameState.YardsToFirstDown - yardGain
                    PlaysRemaining = gameState.PlaysRemaining - 1
                    TeamOneScore = gameState.TeamOneScore
                    TeamTwoScore = gameState.TeamTwoScore
                }
                printfn ""
                PlayGame newState
        else
            printfn "Invalid selection."
            printfn ""
            PlayGame gameState

PlayGame initState
Console.ReadKey() |> ignore