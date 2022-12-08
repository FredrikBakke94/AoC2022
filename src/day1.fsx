module Day1

open System.IO

let readLines (filePath: string) =
    seq {
        use sr = new StreamReader(filePath)

        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

let rec findSums (lines) (curr: int) (groupedSums: list<int>) =
    match lines with
    | "" :: tail -> findSums tail 0 (curr :: groupedSums)
    | head :: tail -> findSums tail (int head + curr) groupedSums
    | [] -> curr :: groupedSums

let day1 (filePath: string) =
    let lines = readLines filePath |> Seq.toList
    let ordered = findSums lines 0 [] |> List.sortDescending
    List.sum ordered[0..2]
