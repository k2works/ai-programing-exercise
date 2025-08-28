module FizzBuzz

let private isDivisibleBy divisor number = number % divisor = 0

// オブジェクト指向アプローチ: FizzBuzzクラス
type FizzBuzz(max: int) =
    let getValue number =
        let isDivisibleBy3 = isDivisibleBy 3 number
        let isDivisibleBy5 = isDivisibleBy 5 number
        
        match (isDivisibleBy3, isDivisibleBy5) with
        | (true, true) -> "FizzBuzz"
        | (true, false) -> "Fizz"
        | (false, true) -> "Buzz"
        | (false, false) -> string number
    
    let _list = [|1..max|] |> Array.map getValue
    
    member this.List = _list

// 後方互換性のための関数版
let fizz_buzz number =
    let isDivisibleBy3 = isDivisibleBy 3 number
    let isDivisibleBy5 = isDivisibleBy 5 number
    
    match (isDivisibleBy3, isDivisibleBy5) with
    | (true, true) -> "FizzBuzz"
    | (true, false) -> "Fizz"
    | (false, true) -> "Buzz"
    | (false, false) -> string number

let create_numbers () = [| 1..100 |]

let create_fizz_buzz_list () =
    let fizzBuzz = FizzBuzz(100)
    fizzBuzz.List

let print_1_to_100 () =
    create_fizz_buzz_list () |> Array.iter (printfn "%s")
