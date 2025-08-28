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

[<Fact>]
let ``タイプ1の場合_3でも5でも割り切れない数値を返す`` () =
    fizz_buzz (1) |> should equal "1"
    fizz_buzz (2) |> should equal "2"
    fizz_buzz (4) |> should equal "4"

[<Fact>]
let ``タイプ2の場合_3で割り切れる数値を返す`` () =
    fizz_buzz (3) |> should equal "Fizz"
    fizz_buzz (6) |> should equal "Fizz" 
    fizz_buzz (9) |> should equal "Fizz"

[<Fact>]
let ``タイプ3の場合_5で割り切れる数値を返す`` () =
    fizz_buzz (5) |> should equal "Buzz"
    fizz_buzz (10) |> should equal "Buzz"
    fizz_buzz (20) |> should equal "Buzz"

[<Fact>]
let ``タイプその他の場合_15で割り切れる数値を返す`` () =
    fizz_buzz (15) |> should equal "FizzBuzz"
    fizz_buzz (30) |> should equal "FizzBuzz"
    fizz_buzz (45) |> should equal "FizzBuzz"

[<Fact>]
let ``FizzBuzzクラス_配列を生成する`` () =
    let fizzBuzz = FizzBuzz(10)
    let result = fizzBuzz.List
    result.Length |> should equal 10
    result.[0] |> should equal "1"
    result.[2] |> should equal "Fizz"
    result.[4] |> should equal "Buzz"
