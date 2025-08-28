module FizzBuzz.Tests

open Xunit
open FsUnit.Xunit
open FizzBuzz

[<Fact>]
let ``数を文字列にして返す`` () = fizz_buzz (1) |> should equal "1"

[<Fact>]
let ``3を渡したら文字列Fizzを返す`` () = fizz_buzz (3) |> should equal "Fizz"

[<Fact>]
let ``5を渡したら文字列Buzzを返す`` () = fizz_buzz (5) |> should equal "Buzz"

[<Fact>]
let ``15を渡したら文字列FizzBuzzを返す`` () =
    fizz_buzz (15) |> should equal "FizzBuzz"

[<Fact>]
let ``1から100までの数を返す`` () =
    let numbers = create_numbers ()
    numbers.[0] |> should equal 1
    numbers.[99] |> should equal 100
    numbers.Length |> should equal 100

[<Fact>]
let ``1から100までのFizzBuzzの配列を返す`` () =
    let result = create_fizz_buzz_list ()
    result.[0] |> should equal "1"
    result.[2] |> should equal "Fizz"
    result.[4] |> should equal "Buzz"
    result.[14] |> should equal "FizzBuzz"
    result.Length |> should equal 100
