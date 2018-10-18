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

let UpdateGameState gameState yardGain = 
    if yardGain >= 100 - gameState.YardLine
    then
        let newState = {
            Possession = not gameState.Possession
            CurrentDown = 1
            YardLine = 25
            YardsToFirstDown = 10
            PlaysRemaining = gameState.PlaysRemaining - 1
            TeamOneScore = if gameState.Possession then gameState.TeamOneScore + 7 else gameState.TeamOneScore
            TeamTwoScore = if gameState.Possession then gameState.TeamTwoScore else gameState.TeamTwoScore + 7
        }
        newState
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
        newState
    else if gameState.CurrentDown = 4 && yardGain < gameState.YardsToFirstDown
    then
        let newState = {
            Possession = not gameState.Possession
            CurrentDown = 1
            YardLine = 100 - (gameState.YardLine + yardGain)
            YardsToFirstDown = 10
            PlaysRemaining = gameState.PlaysRemaining - 1
            TeamOneScore = gameState.TeamOneScore
            TeamTwoScore = gameState.TeamTwoScore
        }
        newState
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
        newState

let ExecutePlay gameState chosenPlay = 
    if (gameState.Possession && chosenPlay = "1") || (not gameState.Possession && chosenPlay = "2")
    then
        printfn "Executing run play"
        let yardGain = rand.Next(0,15) - 5
        printfn "%i yards on the play!" yardGain
        UpdateGameState gameState yardGain
    else if (gameState.Possession && chosenPlay = "2") || (not gameState.Possession && chosenPlay = "1")
    then
        printfn "Executing pass play"
        let yardGain = rand.Next(0, 55) - 15
        printfn "%i yards on the play!" yardGain
        UpdateGameState gameState yardGain
    else
        printfn "Invalid selection."
        printfn ""
        gameState

let rec PlayGame gameState = 

    if gameState.PlaysRemaining = 0
    then
        printfn "Game over! Final Score: "
        printfn "Team One: %i" gameState.TeamOneScore
        printfn "Team Two: %i" gameState.TeamTwoScore
        printfn ""
        printfn "Press any key to exit"
        Console.ReadKey() |> ignore
        exit 0

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
    printfn ""

    if gameState.Possession
    then
        printfn "Offense: Would you like to run or pass?"
        printfn "1. Run"
        printfn "2. Pass"
        PlayGame (ExecutePlay gameState (Console.ReadLine()))
        
    else
        printfn "Defense: Would you like to defend against run or pass?"
        printfn "1. Run"
        printfn "2. Pass"
        PlayGame (ExecutePlay gameState (Console.ReadLine()))

PlayGame initState