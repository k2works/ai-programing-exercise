module FizzBuzz

let private isDivisibleBy divisor number = number % divisor = 0

// Value Object: FizzBuzzValue
type FizzBuzzValue(value: string) =
    member this.Value = value
    override this.ToString() = value
    override this.Equals(other) =
        match other with
        | :? FizzBuzzValue as other -> this.Value = other.Value
        | _ -> false
    override this.GetHashCode() = value.GetHashCode()

// スーパークラスの抽出: 基底クラスと各タイプのクラス
[<AbstractClass>]
type FizzBuzzType() =
    abstract member ToValue: int -> FizzBuzzValue
    abstract member IsMatch: int -> bool

type FizzBuzzType01() =
    inherit FizzBuzzType()
    static let instance = lazy (FizzBuzzType01())
    static member Instance = instance.Value
    override this.ToValue(number) = FizzBuzzValue(string number)
    override this.IsMatch(number) = not (isDivisibleBy 3 number || isDivisibleBy 5 number)

type FizzBuzzType02() =
    inherit FizzBuzzType()
    static let instance = lazy (FizzBuzzType02())
    static member Instance = instance.Value
    override this.ToValue(number) = FizzBuzzValue("Fizz")
    override this.IsMatch(number) = isDivisibleBy 3 number && not (isDivisibleBy 5 number)

type FizzBuzzType03() =
    inherit FizzBuzzType()
    static let instance = lazy (FizzBuzzType03())
    static member Instance = instance.Value
    override this.ToValue(number) = FizzBuzzValue("Buzz")
    override this.IsMatch(number) = isDivisibleBy 5 number && not (isDivisibleBy 3 number)

type FizzBuzzType04() =
    inherit FizzBuzzType()
    static let instance = lazy (FizzBuzzType04())
    static member Instance = instance.Value
    override this.ToValue(number) = FizzBuzzValue("FizzBuzz")
    override this.IsMatch(number) = isDivisibleBy 3 number && isDivisibleBy 5 number

// オブジェクト指向アプローチ: FizzBuzzクラス
type FizzBuzz(max: int) =
    let allTypes = [
        FizzBuzzType04.Instance :> FizzBuzzType
        FizzBuzzType02.Instance :> FizzBuzzType
        FizzBuzzType03.Instance :> FizzBuzzType
        FizzBuzzType01.Instance :> FizzBuzzType
    ]
    
    let getType number =
        allTypes |> List.find (fun t -> t.IsMatch(number))
    
    let getValue number =
        let typeInstance = getType number
        typeInstance.ToValue(number)
    
    let _list = [|1..max|] |> Array.map (getValue >> (fun v -> v.Value))
    
    member this.List = _list

// 後方互換性のための関数版
let fizz_buzz number =
    let allTypes = [
        FizzBuzzType04.Instance :> FizzBuzzType
        FizzBuzzType02.Instance :> FizzBuzzType
        FizzBuzzType03.Instance :> FizzBuzzType
        FizzBuzzType01.Instance :> FizzBuzzType
    ]
    let typeInstance = allTypes |> List.find (fun t -> t.IsMatch(number))
    let value = typeInstance.ToValue(number)
    value.Value

let create_numbers () = [| 1..100 |]

let create_fizz_buzz_list () =
    let fizzBuzz = FizzBuzz(100)
    fizzBuzz.List

let print_1_to_100 () =
    create_fizz_buzz_list () |> Array.iter (printfn "%s")
