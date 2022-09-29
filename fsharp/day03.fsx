let examplePath = "../input/day03.example"
let realPath = "../input/day03.real"


let parse (input: string) = input.Split [| '\n' |] |> Array.toList

let flip bit =
    match bit with
    | '0' -> '1'
    | '1' -> '0'
    | _ -> failwithf "not a bit: %c" bit

let bitsToNum (bits: list<char>) =
    System.Convert.ToInt32(System.String.Join("", bits), 2)

let compareBitCounts (b, n) (b', n') =
    if (n = n') then compare b' b else compare n n'

let findMostCommonBit index binNums =
    binNums
    |> List.countBy (fun (bits: string) -> bits[index])
    |> List.sortWith compareBitCounts
    |> List.maxBy snd
    |> fst


let q1 (input: string) =
    let binNums = parse input

    let commonBits =
        [ for i in 1 .. binNums.[0].Length -> findMostCommonBit (i - 1) binNums ]

    let gamma = commonBits |> bitsToNum
    let epsilon = commonBits |> List.map flip |> bitsToNum
    gamma * epsilon


let findRating (criteria: int -> list<string> -> char) (binNums: list<string>) =
    let rec go nums i =
        if (List.length nums = 1) then
            nums[0]
        else
            let commonBit = criteria i nums
            let nums' = nums |> List.filter (fun num -> num[i] = commonBit)
            go nums' (i + 1)

    let binNum = go binNums 0
    System.Convert.ToInt32(binNum, 2)


let q2 (input: string) =
    let binNums = parse input
    let oxygenRating = findRating findMostCommonBit binNums
    let co2Rating = findRating (fun i nums -> findMostCommonBit i nums |> flip) binNums

    oxygenRating * co2Rating


printfn "q1 example: %A" <| q1 (System.IO.File.ReadAllText examplePath)
printfn "q1 real: %A" <| q1 (System.IO.File.ReadAllText realPath)
printfn "q2 example: %A" <| q2 (System.IO.File.ReadAllText examplePath)
printfn "q2 real: %A" <| q2 (System.IO.File.ReadAllText realPath)
