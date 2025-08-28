module FizzBuzz

let private isDivisibleBy divisor number = number % divisor = 0

// 関数型アプローチ: 判別共用体でFizzBuzzの種類を表現
type FizzBuzzType =
    | Number of int
    | Fizz
    | Buzz
    | FizzBuzz

// 関数型アプローチ: パターンマッチングで変換
let classifyNumber number =
    match (isDivisibleBy 3 number), (isDivisibleBy 5 number) with
    | true, true -> FizzBuzz
    | true, false -> Fizz
    | false, true -> Buzz
    | false, false -> Number number

let fizzBuzzToString = function
    | Number n -> string n
    | Fizz -> "Fizz"
    | Buzz -> "Buzz"
    | FizzBuzz -> "FizzBuzz"

// Value Object: FizzBuzzValue (後方互換性のために残す)
type FizzBuzzValue(value: string) =
    member this.Value = value
    override this.ToString() = value
    override this.Equals(other) =
        match other with
        | :? FizzBuzzValue as other -> this.Value = other.Value
        | _ -> false
    override this.GetHashCode() = value.GetHashCode()

// オブジェクト指向アプローチ (後方互換性のために残す)
[<AbstractClass>]
type FizzBuzzType_OO() =
    abstract member ToValue: int -> FizzBuzzValue
    abstract member IsMatch: int -> bool

type FizzBuzzType01() =
    inherit FizzBuzzType_OO()
    static let instance = lazy (FizzBuzzType01())
    static member Instance = instance.Value
    override this.ToValue(number) = FizzBuzzValue(string number)
    override this.IsMatch(number) = not (isDivisibleBy 3 number || isDivisibleBy 5 number)

type FizzBuzzType02() =
    inherit FizzBuzzType_OO()
    static let instance = lazy (FizzBuzzType02())
    static member Instance = instance.Value
    override this.ToValue(number) = FizzBuzzValue("Fizz")
    override this.IsMatch(number) = isDivisibleBy 3 number && not (isDivisibleBy 5 number)

type FizzBuzzType03() =
    inherit FizzBuzzType_OO()
    static let instance = lazy (FizzBuzzType03())
    static member Instance = instance.Value
    override this.ToValue(number) = FizzBuzzValue("Buzz")
    override this.IsMatch(number) = isDivisibleBy 5 number && not (isDivisibleBy 3 number)

type FizzBuzzType04() =
    inherit FizzBuzzType_OO()
    static let instance = lazy (FizzBuzzType04())
    static member Instance = instance.Value
    override this.ToValue(number) = FizzBuzzValue("FizzBuzz")
    override this.IsMatch(number) = isDivisibleBy 3 number && isDivisibleBy 5 number

// 関数型アプローチ: 関数合成とパイプライン演算子
let fizz_buzz_functional = classifyNumber >> fizzBuzzToString

// 関数型アプローチ: リスト生成
let createFizzBuzzList max = 
    [1..max] 
    |> List.map fizz_buzz_functional
    |> List.toArray

// 関数型アプローチ: FizzBuzzクラス (関数型実装)
type FizzBuzzFunctional(max: int) =
    let _list = createFizzBuzzList max
    member this.List = _list

// オブジェクト指向アプローチ: FizzBuzzクラス (後方互換性のために残す)
type FizzBuzz(max: int) =
    let allTypes = [
        FizzBuzzType04.Instance :> FizzBuzzType_OO
        FizzBuzzType02.Instance :> FizzBuzzType_OO
        FizzBuzzType03.Instance :> FizzBuzzType_OO
        FizzBuzzType01.Instance :> FizzBuzzType_OO
    ]
    
    let getType number =
        allTypes |> List.find (fun t -> t.IsMatch(number))
    
    let getValue number =
        let typeInstance = getType number
        typeInstance.ToValue(number)
    
    let _list = [|1..max|] |> Array.map (getValue >> (fun v -> v.Value))
    
    member this.List = _list

// 関数版: 関数型実装を使用
let fizz_buzz number = fizz_buzz_functional number

// 後方互換性のためのオブジェクト指向版関数
let fizz_buzz_oo number =
    let allTypes = [
        FizzBuzzType04.Instance :> FizzBuzzType_OO
        FizzBuzzType02.Instance :> FizzBuzzType_OO
        FizzBuzzType03.Instance :> FizzBuzzType_OO
        FizzBuzzType01.Instance :> FizzBuzzType_OO
    ]
    let typeInstance = allTypes |> List.find (fun t -> t.IsMatch(number))
    let value = typeInstance.ToValue(number)
    value.Value

// 関数型アプローチの主要な関数群
let create_numbers () = [| 1..100 |]

let create_fizz_buzz_list () = createFizzBuzzList 100

let print_1_to_100 () =
    create_fizz_buzz_list () |> Array.iter (printfn "%s")

// 後方互換性のためのオブジェクト指向版関数
let create_fizz_buzz_list_oo () =
    let fizzBuzz = FizzBuzz(100)
    fizzBuzz.List
