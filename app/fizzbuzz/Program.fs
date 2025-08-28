open NUnit.Framework

let greeting() = "hello world"

[<TestFixture>]
type HelloTest() =

    [<Test>]
    member __.TestGreeting() =
        Assert.That(greeting(), Is.EqualTo("hello world"))

[<EntryPoint>]
let main argv =
    printfn "Hello from F#"
    0