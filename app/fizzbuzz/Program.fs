open NUnit.Framework

let to_string number = 
    if number % 3 = 0 then "Fizz" 
    else string number

let fizz_buzz number = to_string number

[<TestFixture>]
type FizzBuzzTest() =

    [<Test>]
    member __.Test数を文字列にして返す() =
        Assert.That(fizz_buzz(1), Is.EqualTo("1"))

    [<Test>]
    member __.Test3を渡したら文字列Fizzを返す() =
        Assert.That(fizz_buzz(3), Is.EqualTo("Fizz"))

[<EntryPoint>]
let main argv =
    printfn "Hello from F#"
    0