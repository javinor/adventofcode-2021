let examplePath = "../input/day06.example"
let realPath = "../input/day06.real"

let addOrElse n =
    function
    | None -> Some n
    | Some k -> Some(k + n)

let mkOccuranceMap map key = Map.change key (addOrElse (bigint 1)) map

let parse (input: string) =
    input.Split "," |> Array.map int |> Array.fold mkOccuranceMap Map.empty

let tick timers =
    Map.fold
        (fun timers' timer n ->
            match timer with
            | 0 -> timers' |> Map.add 8 n |> Map.change 6 (addOrElse n)
            | _ -> timers' |> Map.change (timer - 1) (addOrElse n))
        Map.empty
        timers

let rec run n timers =
    if n = 0 then timers else run (n - 1) (tick timers) 

let q1 (input: string) = 
    parse input |> run 80 |> Map.fold (fun acc _ n -> acc + n) (bigint 0)


let q2 (input: string) = 
    parse input |> run 256 |> Map.fold (fun acc _ n -> acc + n) (bigint 0)


printfn "q1 example: %A" <| q1 (System.IO.File.ReadAllText examplePath)
printfn "q1 real: %A" <| q1 (System.IO.File.ReadAllText realPath)
printfn "q2 example: %A" <| q2 (System.IO.File.ReadAllText examplePath)
printfn "q2 real: %A" <| q2 (System.IO.File.ReadAllText realPath)
