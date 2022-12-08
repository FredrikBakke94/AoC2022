module Day2

[<Literal>]
let OpponentRock = "A"

[<Literal>]
let YourRock = "X"

[<Literal>]
let OpponentPaper = "B"

[<Literal>]
let YourPaper = "Y"

[<Literal>]
let OpponentScissors = "C"

[<Literal>]
let YourScissors = "Z"

[<Literal>]
let Draw = "Y"

[<Literal>]
let Win = "Z"

[<Literal>]
let Lose = "X"


let rec findScoreA lines score =
    match lines with
    | (OpponentRock, YourScissors) :: tail -> findScoreA tail score + 3
    | (OpponentRock, YourPaper) :: tail -> findScoreA tail score + 8
    | (OpponentRock, YourRock) :: tail -> findScoreA tail score + 4
    | (OpponentPaper, YourScissors) :: tail -> findScoreA tail score + 9
    | (OpponentPaper, YourPaper) :: tail -> findScoreA tail score + 5
    | (OpponentPaper, YourRock) :: tail -> findScoreA tail score + 1
    | (OpponentScissors, YourScissors) :: tail -> findScoreA tail score + 6
    | (OpponentScissors, YourPaper) :: tail -> findScoreA tail score + 2
    | (OpponentScissors, YourRock) :: tail -> findScoreA tail score + 7
    | [] -> score

let rec findScoreB lines score =
    match lines with
    | (OpponentRock, Lose) :: tail -> findScoreB tail score + 3
    | (OpponentRock, Win) :: tail -> findScoreB tail score + 8
    | (OpponentRock, Draw) :: tail -> findScoreB tail score + 4
    | (OpponentPaper, Win) :: tail -> findScoreB tail score + 9
    | (OpponentPaper, Draw) :: tail -> findScoreB tail score + 5
    | (OpponentPaper, Lose) :: tail -> findScoreB tail score + 1
    | (OpponentScissors, Draw) :: tail -> findScoreB tail score + 6
    | (OpponentScissors, Lose) :: tail -> findScoreB tail score + 2
    | (OpponentScissors, Win) :: tail -> findScoreB tail score + 7
    | [] -> score


let day2 (filePath: string) =
    let lines =
        ReadFile.readLines filePath
        |> List.map (fun x ->
            match (x.Split(" ") |> Array.toList) with
            | opp :: you :: tail -> (opp, you))

    printfn $"{lines}"

    let scoreA = findScoreA lines 0

    printfn $"Your score: {scoreA}"

    let scoreB = findScoreB lines 0

    printfn $"Score b: {scoreB}"
