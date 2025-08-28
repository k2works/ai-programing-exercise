module FizzBuzz

let private isDivisibleBy divisor number = number % divisor = 0

let private isFizzBuzzNumber number =
    (isDivisibleBy 3 number) && (isDivisibleBy 5 number)

let private isFizzNumber number = isDivisibleBy 3 number

let private isBuzzNumber number = isDivisibleBy 5 number

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
    create_numbers () |> Array.map fizz_buzz

let print_1_to_100 () =
    create_fizz_buzz_list () |> Array.iter (printfn "%s")
