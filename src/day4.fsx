module Day4

let rec part1 data operlaps =
    match data with
    | (elf1, elf2) :: tail ->
        let e1InE2 =
            elf1
            |> List.forall (fun area -> elf2 |> List.exists (fun area2 -> area2.Equals(area)))

        let e2InE1 =
            elf2
            |> List.forall (fun area -> elf1 |> List.exists (fun area2 -> area2.Equals(area)))

        if e1InE2 || e2InE1 then
            part1 tail operlaps + 1
        else
            part1 tail operlaps
    | [] -> operlaps


let rec part2 data operlaps =
    match data with
    | (elf1, elf2) :: tail ->
        let e1InE2 =
            elf1
            |> List.exists (fun area -> elf2 |> List.exists (fun area2 -> area2.Equals(area)))

        let e2InE1 =
            elf2
            |> List.exists (fun area -> elf1 |> List.exists (fun area2 -> area2.Equals(area)))

        if e1InE2 || e2InE1 then
            part2 tail operlaps + 1
        else
            part2 tail operlaps
    | [] -> operlaps


let day4 filePath =
    let lines =
        ReadFile.readLines filePath
        |> List.map (fun line ->
            let [| elf1; elf2 |] = line.Split(',')
            let [| e1Lower; e1Upper |] = elf1.Split('-')
            let [| e2Lower; e2Upper |] = elf2.Split('-')
            ([ (int e1Lower) .. (int e1Upper) ], [ (int e2Lower) .. (int e2Upper) ]))


    printfn $"{lines}"

    let res1 = part1 lines 0
    printfn $"Part1: {res1}"

    let res2 = part2 lines 0
    printfn $"Part2: {res2}"
