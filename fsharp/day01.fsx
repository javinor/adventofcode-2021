let examplePath = "../input/day01.example"
let realPath = "../input/day01.real"

let q1 (input: string) =
    input.Split [| '\n' |]
    |> Array.toList
    |> List.map int
    |> List.pairwise
    |> List.map (fun (x, y) -> if y > x then 1 else 0)
    |> List.sum

let q2 (input: string) =
    input.Split [| '\n' |]
    |> Array.toList
    |> List.map int
    |> List.windowed 3
    |> List.map (List.sum)
    |> List.pairwise
    |> List.map (fun (x, y) -> if y > x then 1 else 0)
    |> List.sum

printfn "q1 example: %A" <| q1 (System.IO.File.ReadAllText examplePath)
printfn "q1 real: %A" <| q1 (System.IO.File.ReadAllText realPath)
printfn "q2 example: %A" <| q2 (System.IO.File.ReadAllText examplePath)
printfn "q2 real: %A" <| q2 (System.IO.File.ReadAllText realPath)
