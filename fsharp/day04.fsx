let examplePath = "../input/day04.example"
let realPath = "../input/day04.real"

type Marking =
    | Marked
    | Unmarked

type Card = (Marking * string) list list

let parseNumbers (nums: string) = nums.Split "," |> Array.toList

let notEmpty c = c <> ""

let toCard x = (Unmarked, x)

let parseCardRow (row: string) =
    row.Split " " |> Array.filter notEmpty |> Array.map toCard |> Array.toList

let parseBingoCard (card: string) =
    card.Split "\n" |> Array.toList |> List.map parseCardRow

let parse (input: string) : string list * Card list =
    match input.Split "\n\n" |> Array.toList with
    | nums :: cards -> (parseNumbers nums, cards |> List.map parseBingoCard)
    | _ -> failwith "bad input"

//////////////////////

let id x = x

let markCell x (marking, x') =
    if x = x' then (Marked, x') else (marking, x')

let markCardRow x row = row |> List.map (markCell x)

let mark x (cards: Card list) =
    cards |> List.map (fun card -> card |> List.map (markCardRow x))


let isMarked (marking, _) = marking = Marked
let rowIsMarked row = row |> List.forall isMarked

let cardIsAWinner card =
    (card |> List.exists rowIsMarked
     || card |> List.transpose |> List.exists rowIsMarked)

let checkWinner cards =
    cards
    |> List.fold
        (fun winner card ->
            match winner with
            | Some _ -> winner
            | None -> if (cardIsAWinner card) then Some card else None)
        None


let rec playBingo nums cards =
    let (x, xs) =
        match nums with
        | [] -> failwith "out of numbers for bingo"
        | x :: xs -> (x, xs)

    let cards' = mark x cards

    match checkWinner cards' with
    | None -> playBingo xs cards'
    | Some card -> (x, card)


let q1 (input: string) =
    let (nums, cards) = parse input
    let (num, card) = playBingo nums cards

    let sum = card |> List.collect id |> List.filter (not << isMarked) |> List.map (snd >> int) |> List.sum
    (int num) * sum


let rec playBingo2 nums cards =
    let (x, xs) =
        match nums with
        | [] -> failwith "out of numbers for bingo"
        | x :: xs -> (x, xs)

    let cards' = mark x cards |> List.filter (not << cardIsAWinner)
    match cards' with
    | [] -> failwith "impossible state - no cards left"
    | [c] -> playBingo xs [c]
    | _ -> playBingo2 xs cards'


let q2 (input: string) = 
    let (nums, cards) = parse input
    let (num, card) = playBingo2 nums cards
    let sum = card |> List.collect id |> List.filter (not << isMarked) |> List.map (snd >> int) |> List.sum
    (int num) * sum


printfn "q1 example: %A" <| q1 (System.IO.File.ReadAllText examplePath)
printfn "q1 real: %A" <| q1 (System.IO.File.ReadAllText realPath)
printfn "q2 example: %A" <| q2 (System.IO.File.ReadAllText examplePath)
printfn "q2 real: %A" <| q2 (System.IO.File.ReadAllText realPath)
