open NUnit.Framework

let to_string number = "1"

let fizz_buzz number = to_string number

[<TestFixture>]
type FizzBuzzTest() =

    [<Test>]
    member __.Test数を文字列にして返す() =
        Assert.That(fizz_buzz(1), Is.EqualTo("1"))

[<EntryPoint>]
let main argv =
    printfn "Hello from F#"
    0