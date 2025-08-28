open NUnit.Framework

let to_string number = 
    if number % 3 = 0 && number % 5 = 0 then "FizzBuzz"
    elif number % 3 = 0 then "Fizz" 
    elif number % 5 = 0 then "Buzz"
    else string number

let fizz_buzz number = to_string number

let create_numbers() = [|1..100|]

let create_fizz_buzz_list() = 
    create_numbers()
    |> Array.map fizz_buzz

[<TestFixture>]
type FizzBuzzTest() =

    [<Test>]
    member __.Test数を文字列にして返す() =
        Assert.That(fizz_buzz(1), Is.EqualTo("1"))

    [<Test>]
    member __.Test3を渡したら文字列Fizzを返す() =
        Assert.That(fizz_buzz(3), Is.EqualTo("Fizz"))

    [<Test>]
    member __.Test5を渡したら文字列Buzzを返す() =
        Assert.That(fizz_buzz(5), Is.EqualTo("Buzz"))

    [<Test>]
    member __.Test15を渡したら文字列FizzBuzzを返す() =
        Assert.That(fizz_buzz(15), Is.EqualTo("FizzBuzz"))

    [<Test>]
    member __.Test1から100までの数を返す() =
        let numbers = create_numbers()
        Assert.That(numbers.[0], Is.EqualTo(1))
        Assert.That(numbers.[99], Is.EqualTo(100))
        Assert.That(numbers.Length, Is.EqualTo(100))

    [<Test>]
    member __.Test1から100までのFizzBuzzの配列を返す() =
        let result = create_fizz_buzz_list()
        Assert.That(result.[0], Is.EqualTo("1"))
        Assert.That(result.[2], Is.EqualTo("Fizz"))
        Assert.That(result.[4], Is.EqualTo("Buzz"))
        Assert.That(result.[14], Is.EqualTo("FizzBuzz"))
        Assert.That(result.Length, Is.EqualTo(100))

[<EntryPoint>]
let main argv =
    printfn "Hello from F#"
    0