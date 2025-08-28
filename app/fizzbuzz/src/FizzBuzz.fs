module FizzBuzz

let private isDivisibleBy divisor number = number % divisor = 0

// スーパークラスの抽出: 基底クラスと各タイプのクラス
[<AbstractClass>]
type FizzBuzzType() =
    abstract member Generate: int -> string

type FizzBuzzType01() =
    inherit FizzBuzzType()
    static let instance = lazy (FizzBuzzType01())
    static member Instance = instance.Value
    override this.Generate(number) = string number

type FizzBuzzType02() =
    inherit FizzBuzzType()
    static let instance = lazy (FizzBuzzType02())
    static member Instance = instance.Value
    override this.Generate(number) = "Fizz"

type FizzBuzzType03() =
    inherit FizzBuzzType()
    static let instance = lazy (FizzBuzzType03())
    static member Instance = instance.Value
    override this.Generate(number) = "Buzz"

type FizzBuzzType04() =
    inherit FizzBuzzType()
    static let instance = lazy (FizzBuzzType04())
    static member Instance = instance.Value
    override this.Generate(number) = "FizzBuzz"

// オブジェクト指向アプローチ: FizzBuzzクラス
type FizzBuzz(max: int) =
    let getType number =
        let isDivisibleBy3 = isDivisibleBy 3 number
        let isDivisibleBy5 = isDivisibleBy 5 number
        
        match (isDivisibleBy3, isDivisibleBy5) with
        | (true, true) -> FizzBuzzType04.Instance :> FizzBuzzType
        | (true, false) -> FizzBuzzType02.Instance :> FizzBuzzType
        | (false, true) -> FizzBuzzType03.Instance :> FizzBuzzType
        | (false, false) -> FizzBuzzType01.Instance :> FizzBuzzType
    
    let getValue number =
        let typeInstance = getType number
        typeInstance.Generate(number)
    
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
