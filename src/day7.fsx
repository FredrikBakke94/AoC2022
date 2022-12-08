module Day7

type FileSysTypes =
    | File of string * int
    | Dir of string

type Direc = list<FileSysTypes>

type DirMap = Map<string, Direc>

let rec buildFileSys (lines: list<string[]>) (dirMap: DirMap) (path: string[]) =

    match lines with
    | [| "$"; "cd"; ".." |] :: tail -> buildFileSys tail dirMap (Array.take (path.Length - 1) path)
    | [| "$"; "cd"; dirName |] :: [| "$"; "ls" |] :: tail ->
        let curPath =
            (String.concat "/" (Array.append path [| dirName |])).Replace("//", "/")

        let dirContents =
            tail
            |> List.takeWhile (fun line -> line[0] <> "$")
            |> List.map (fun line ->
                match line with
                | [| "dir"; name |] -> Dir(($"{curPath}/{name}".Replace("//", "/")))
                | [| size; fileName |] -> File(fileName, int size))

        buildFileSys
            (List.skip dirContents.Length tail)
            (dirMap.Add(curPath, dirContents))
            (Array.append path [| dirName |])
    | [] -> dirMap


let rec dirSize (dir: Direc) (dirMap: DirMap) =
    List.fold
        (fun state item ->
            match item with
            | File(name, size) -> state + size
            | Dir(name) -> state + dirSize (dirMap.Item name) dirMap)
        0
        dir

let day7 filePath =
    let lines = ReadFile.readLines filePath |> List.map (fun line -> line.Split(" "))

    // printfn $"{lines}"

    let dirMap = buildFileSys lines Map.empty [||]
    // printfn $"{dirMap}"

    let keys = Map.keys dirMap |> Seq.toList
    printfn $"{keys} {keys.Length}"

    // for (key, v) in (Map.toList dirMap) do
    //     printfn $"{key}: {v}"

    let dirSizes = Map.map (fun key v -> dirSize v dirMap) dirMap

    let res1 =
        dirSizes
        |> Map.fold (fun state key v -> if v < 100000 then state + v else state) 0

    printfn $"part1: {res1}"

    let spaceFree = 70000000 - (dirSizes.Item "/")
    printfn $"Space free: {spaceFree}"

    let freeUpSize = 30000000 - spaceFree
    printfn $"Space needed: {freeUpSize}"

    let deleteDir =
        dirSizes
        |> Map.filter (fun key v -> v >= freeUpSize)
        |> Map.toList
        |> List.minBy (fun (k, v) -> v)

    printfn $"Delete Dir: {deleteDir}"
