module Day5

let rec getColValues (line: string) =
    let crate = line |> Seq.take 3
    let createMark = crate |> Seq.item 1

    if line.Length.Equals(3) then
        [ createMark ]
    else
        createMark :: (getColValues (line.Substring(4)))

let getMoves (line: string) =
    let [| move; amount; from; stack; toStack; dest |] = line.Split(' ')
    ((int amount), (int stack - 1), (int dest - 1))

let rec part1 (stacks: list<char>[]) (moves: list<(int * int * int)>) =
    match moves with
    | (amount, fromStack, toStack) :: tail ->
        let moveItems = List.take amount stacks[fromStack]
        let destStack = List.append moveItems stacks[toStack]

        let updatedStacks =
            stacks
            |> Array.mapi (fun i stack ->
                match i with
                | stackId when stackId = fromStack -> List.skip amount stacks[stackId]
                | stackId when stackId = toStack -> destStack
                | _ -> stack)

        printfn $"Move {amount} from {fromStack} to {toStack} \n {Array.toList updatedStacks}"

        part1 updatedStacks tail
    | [] ->
        stacks
        |> Array.toSeq
        |> Seq.map (List.head)
        |> Seq.map (fun item -> string item)
        |> String.concat ""

let day5 filePath =
    let lines = ReadFile.readLines filePath

    let stackInput =
        lines
        |> List.takeWhile (fun line -> line.Length > 1)
        |> List.rev
        |> List.skip 1
        |> List.rev


    let stacks =
        stackInput
        |> List.map (getColValues)
        |> List.transpose
        |> List.map (fun stack -> List.filter (fun item -> item <> ' ') stack)
        |> List.toArray


    printfn $"Start: {Array.toList stacks}"

    let moves =
        lines
        |> List.skipWhile (fun line -> line.Length > 1)
        |> List.skip 1
        |> List.map (getMoves)

    printfn $"{moves} length: {moves.Length}"

    let res1 = part1 stacks moves

    printfn $"Part1: {res1}"
