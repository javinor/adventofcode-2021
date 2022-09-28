let examplePath = "../input/day02.example"
let realPath = "../input/day02.real"

type Command =
    | Forward
    | Down
    | Up

let parseCommand str =
    match str with
    | "forward" -> Forward
    | "up" -> Up
    | "down" -> Down
    | _ -> failwithf "unknown command: %s" str


let parseLine (line: string) =
    match line.Split [| ' ' |] with
    | [| cmd; n |] -> (parseCommand cmd, int n)
    | err -> failwithf "illegal line: %A" err

let parse (input: string) = 
    input.Split [| '\n' |] 
    |> Array.toList
    |> List.map parseLine


let q1 (input: string) = 
    parse input
    |> List.fold 
        (fun (horizontal, depth) (command, n) -> 
            match command with
            | Forward -> (horizontal + n, depth)
            | Up -> (horizontal, depth - n)
            | Down -> (horizontal, depth + n)
        )
        (0,0)
    |> fun (x, y) -> x * y

let q2 (input: string) = 
    parse input
    |> List.fold 
        (fun (horizontal, depth, aim) (command, n) -> 
            match command with
            | Forward -> (horizontal + n, depth + n * aim, aim)
            | Up -> (horizontal, depth, aim - n)
            | Down -> (horizontal, depth, aim + n)
        )
        (0,0,0)
    |> fun (x, y, _) -> x * y


printfn "q1 example: %A" <| q1 (System.IO.File.ReadAllText examplePath)
printfn "q1 real: %A" <| q1 (System.IO.File.ReadAllText realPath)
printfn "q2 example: %A" <| q2 (System.IO.File.ReadAllText examplePath)
printfn "q2 real: %A" <| q2 (System.IO.File.ReadAllText realPath)
