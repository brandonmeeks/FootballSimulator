open System
open System.Threading

type GameState = {
    Possession : bool
    CurrentDown : int
    YardLine : int
    YardsToFirstDown : int
    PlaysRemaining : int
    TeamOneScore : int
    TeamTwoScore : int
    }
type Play = Run | Pass

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
    //touchdown
    if yardGain >= 100 - gameState.YardLine then
        {
            Possession = not gameState.Possession
            CurrentDown = 1
            YardLine = 25
            YardsToFirstDown = 10
            PlaysRemaining = gameState.PlaysRemaining - 1
            TeamOneScore = if gameState.Possession then gameState.TeamOneScore + 7 else gameState.TeamOneScore
            TeamTwoScore = if gameState.Possession then gameState.TeamTwoScore else gameState.TeamTwoScore + 7
        }
    //safety
    else if (yardGain + gameState.YardLine) < 1 then
        {
            Possession = not gameState.Possession
            CurrentDown = 1
            YardLine = 35
            YardsToFirstDown = 10
            PlaysRemaining = gameState.PlaysRemaining - 1
            TeamOneScore = if gameState.Possession then gameState.TeamOneScore else gameState.TeamOneScore + 2
            TeamTwoScore = if gameState.Possession then gameState.TeamTwoScore + 2 else gameState.TeamTwoScore
        }
    //first down
    else if yardGain >= gameState.YardsToFirstDown then
        {
            Possession = gameState.Possession
            CurrentDown = 1
            YardLine = gameState.YardLine + yardGain
            YardsToFirstDown = 10
            PlaysRemaining = gameState.PlaysRemaining - 1
            TeamOneScore = gameState.TeamOneScore
            TeamTwoScore = gameState.TeamTwoScore
        }
    //turnover
    else if gameState.CurrentDown = 4 && yardGain < gameState.YardsToFirstDown then
        {
            Possession = not gameState.Possession
            CurrentDown = 1
            YardLine = 100 - (gameState.YardLine + yardGain)
            YardsToFirstDown = 10
            PlaysRemaining = gameState.PlaysRemaining - 1
            TeamOneScore = gameState.TeamOneScore
            TeamTwoScore = gameState.TeamTwoScore
        }
    else
        {
            Possession = gameState.Possession
            CurrentDown = gameState.CurrentDown + 1
            YardLine = gameState.YardLine + yardGain
            YardsToFirstDown = gameState.YardsToFirstDown - yardGain
            PlaysRemaining = gameState.PlaysRemaining - 1
            TeamOneScore = gameState.TeamOneScore
            TeamTwoScore = gameState.TeamTwoScore
        }
let ExecutePlay gameState chosenPlay = 
    //Run offense or Pass defense
    if (gameState.Possession && chosenPlay = Run) || (not gameState.Possession && chosenPlay = Pass) then
        if gameState.Possession then printfn "Executing run play"
        else printfn "Executing pass play"
        let yardGain = rand.Next(0,15) - 5
        printfn "%i yards on the play!" yardGain
        UpdateGameState gameState yardGain
    //Pass offense or Run defense
    else
        if gameState.Possession then printfn "Executing pass play"
        else printfn "Executing run play"
        let yardGain = rand.Next(0, 55) - 15
        printfn "%i yards on the play!" yardGain
        UpdateGameState gameState yardGain
let DrawField possession yardLine = 
    let field = if possession then
                    "|" + String('-',yardLine-1) + ">" + String('-',99-yardLine) + "|"
                else
                    "|" + String('-',99-yardLine) + "<" + String('-',yardLine-1) + "|"
    printfn ""
    printfn "%s" field
    printfn ""
let DisplayGameInfo gameState = 
    printfn "Current Game State:"
    if gameState.Possession then printfn "Possession: Team One"
    else printfn "Possession: Team Two"
    printfn "Current Down: %i" gameState.CurrentDown
    printfn "Yard Line: %i" gameState.YardLine
    printfn "Yards Until First Down: %i" gameState.YardsToFirstDown
    printfn "Number of plays remaining in game: %i" gameState.PlaysRemaining
    printfn "Team One Score: %i" gameState.TeamOneScore
    printfn "Team Two Score: %i" gameState.TeamTwoScore
    DrawField gameState.Possession gameState.YardLine
    if gameState.Possession then printfn "Offense: Would you like to run or pass?"
    else printfn "Defense: Would you like to defend against run or pass?"
    printfn "1. Run"
    printfn "2. Pass"
let EndGame gameState =
    printfn "Game over! Final Score: "
    printfn "Team One: %i" gameState.TeamOneScore
    printfn "Team Two: %i" gameState.TeamTwoScore
    printfn ""
    printfn "Press any key to exit"
    Console.ReadKey() |> ignore
    exit 0
let rec PlayGame gameState = 
    Thread.Sleep(1000)
    Console.Clear()
    if gameState.PlaysRemaining = 0 then EndGame gameState
    DisplayGameInfo gameState

    let input = Console.ReadLine()
    if (input = "1") then PlayGame (ExecutePlay gameState (Run))
    else if (input = "2") then PlayGame (ExecutePlay gameState (Pass))
    else
        printfn "Invalid selection"
        PlayGame gameState
        
PlayGame initState