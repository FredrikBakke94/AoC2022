module Day6

let part1 (dataStream: string) =
    let rec findStart (msg: list<char>) (i: int) =
        // printfn $"{Seq.take 2 msg |> Seq.toList} {Seq.skip 2 msg |> Seq.take 2 |> Seq.toList} {i}"
        // printfn $"{Seq.length msg}"
        let startMsg = List.take 14 msg |> List.distinct

        if (List.length startMsg) = 14 then
            i + 14
        else
            findStart (List.skip 1 msg) (i + 1)

    findStart (Seq.toList dataStream) 0

let day6 filePath =
    let lines = ReadFile.readLines filePath
    printfn $"{lines}"

    let res1 = part1 lines.Head

    printfn $"{res1}"
