open ReadFile

let prioMap = [ 'a' .. 'z' ] @ [ 'A' .. 'Z' ]

let getPrio char =
    (List.findIndex (fun c -> c.Equals(char)) prioMap) + 1

let rec computePriority (lines: list<string>) total =
    match lines with
    | head :: tail ->
        let ruck1 = head[0 .. (head.Length / 2 - 1)]
        let ruck2 = head[head.Length / 2 .. head.Length]

        let commonItem =
            ruck1
            |> Seq.fold
                (fun (state: char option) (item: char) -> if ruck2.Contains(item) then Some(item) else state)
                None

        match commonItem with
        | Some(item) -> computePriority tail (getPrio item) + total
        | None -> failwith "No common item"

    | [] -> total

let rec findBadges (lines: list<string>) total =
    match lines with
    | ruck1 :: ruck2 :: ruck3 :: tail ->
        let commonItem =
            ruck1
            |> Seq.fold
                (fun (state: char option) item ->
                    if ruck2.Contains(item) && ruck3.Contains(item) then
                        Some(item)
                    else
                        state)
                None

        match commonItem with
        | Some(item) -> findBadges tail (getPrio item) + total
        | None -> failwith "No common badge"
    | [] -> total

let day3 filePath =
    let lines = readLines filePath
    printfn $"{lines}"
    let result = computePriority lines 0
    printfn $"{result}"
    let badgeTotal = findBadges lines 0
    printfn $"{badgeTotal}"
