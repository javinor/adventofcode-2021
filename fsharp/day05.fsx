let examplePath = "../input/day05.example"
let realPath = "../input/day05.real"

type Point = int * int
type Line = Line of Point * Point

let parseCoord (coord: string) =
    match coord.Split "," with
    | [| x; y |] -> (int x, int y)
    | _ -> failwithf "failed to parse coord: %s" coord

let parseLine (line: string) =
    match (line.Split " -> " |> Array.map parseCoord) with
    | [| p1; p2 |] -> Line(p1, p2)
    | _ -> failwithf "failed to parse line: %s" line

let parse (input: string) =
    input.Split "\n" |> Array.fold (fun acc line -> parseLine line :: acc) []

///////////////////////

let calcLinePoints (Line ((x, y), (x', y'))) =
    let xs = if (x <= x') then [ x..x' ] else List.rev [ x'..x ]
    let ys = if (y <= y') then [ y..y' ] else List.rev [ y'..y ]

    if (x = x') then [ for v in ys -> (x, v) ]
    elif (y = y') then [ for u in xs -> (u, y) ]
    else List.zip xs ys


let q1 (input: string) =
    parse input
    |> List.filter (fun (Line ((x, y), (x', y'))) -> x = x' || y = y')
    |> List.collect calcLinePoints
    |> List.countBy (fun x -> x)
    |> List.filter (fun (_, n) -> n >= 2)
    |> List.length


let q2 (input: string) =
    parse input
    |> List.fold
        (fun acc line ->
            calcLinePoints line
            |> List.fold
                (fun acc point ->
                    Map.change
                        point
                        (function
                        | None -> Some 1
                        | Some n -> Some(n + 1))
                        acc)
                acc


            )
        Map.empty
    |> Map.filter (fun _ n -> n >= 2)
    |> Map.toList
    |> List.length





printfn "q1 example: %A" <| q1 (System.IO.File.ReadAllText examplePath)
printfn "q1 real: %A" <| q1 (System.IO.File.ReadAllText realPath)
printfn "q2 example: %A" <| q2 (System.IO.File.ReadAllText examplePath)
printfn "q2 real: %A" <| q2 (System.IO.File.ReadAllText realPath)
