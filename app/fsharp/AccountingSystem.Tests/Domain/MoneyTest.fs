module AccountingSystem.Tests.Domain.MoneyTest

open AccountingSystem.Domain.Types
open Xunit
open FsUnit.Xunit
open System

/// <summary>
/// Money 値オブジェクトのテスト
/// </summary>
type MoneyTest() =

    [<Fact>]
    member _.``Money.Zero はゼロ金額を返す``() =
        let zero = Money.Zero
        zero.Amount |> should equal 0m

    [<Fact>]
    member _.``Money.Create で金額を作成できる``() =
        let money = Money.Create(1000m)
        money.Amount |> should equal 1000m

    [<Fact>]
    member _.``Money.Create は負の値も許可する``() =
        let money = Money.Create(-500m)
        money.Amount |> should equal -500m

    [<Fact>]
    member _.``Money.CreatePositive は正の値を作成できる``() =
        let money = Money.CreatePositive(1000m)
        money.Amount |> should equal 1000m

    [<Fact>]
    member _.``Money.CreatePositive は負の値で例外を投げる``() =
        (fun () -> Money.CreatePositive(-100m) |> ignore)
        |> should throw typeof<ArgumentException>

    [<Fact>]
    member _.``Money の加算ができる``() =
        let a = Money.Create(100m)
        let b = Money.Create(200m)
        let result = a + b
        result.Amount |> should equal 300m

    [<Fact>]
    member _.``Money の減算ができる``() =
        let a = Money.Create(500m)
        let b = Money.Create(200m)
        let result = a - b
        result.Amount |> should equal 300m

    [<Fact>]
    member _.``Money のスカラー乗算ができる``() =
        let money = Money.Create(100m)
        let result = money * 3m
        result.Amount |> should equal 300m

    [<Fact>]
    member _.``Money のスカラー除算ができる``() =
        let money = Money.Create(300m)
        let result = money / 3m
        result.Amount |> should equal 100m

    [<Fact>]
    member _.``Money のゼロ除算は例外を投げる``() =
        let money = Money.Create(100m)
        (fun () -> money / 0m |> ignore)
        |> should throw typeof<ArgumentException>

    [<Fact>]
    member _.``Money.Abs は絶対値を返す``() =
        let negative = Money.Create(-500m)
        negative.Abs().Amount |> should equal 500m

    [<Fact>]
    member _.``Money.IsNegative は負の金額を判定する``() =
        let negative = Money.Create(-100m)
        let positive = Money.Create(100m)
        negative.IsNegative |> should equal true
        positive.IsNegative |> should equal false

    [<Fact>]
    member _.``Money.IsPositive は正の金額を判定する``() =
        let positive = Money.Create(100m)
        let negative = Money.Create(-100m)
        positive.IsPositive |> should equal true
        negative.IsPositive |> should equal false

    [<Fact>]
    member _.``Money.IsZero はゼロ金額を判定する``() =
        let zero = Money.Zero
        let nonZero = Money.Create(100m)
        zero.IsZero |> should equal true
        nonZero.IsZero |> should equal false

    [<Fact>]
    member _.``Money.Negate は符号を反転する``() =
        let positive = Money.Create(100m)
        let negative = positive.Negate()
        negative.Amount |> should equal -100m

    [<Fact>]
    member _.``Money.Round は指定桁数に丸める``() =
        let money = Money.Create(123.456m)
        let rounded = money.Round(2)
        rounded.Amount |> should equal 123.46m

    [<Fact>]
    member _.``同じ金額の Money は等しい``() =
        let a = Money.Create(1000m)
        let b = Money.Create(1000m)
        Money.equal a b |> should equal true

    [<Fact>]
    member _.``異なる金額の Money は等しくない``() =
        let a = Money.Create(1000m)
        let b = Money.Create(2000m)
        Money.equal a b |> should equal false

    [<Fact>]
    member _.``Money.sum で金額リストを合計できる``() =
        let moneys = [
            Money.Create(100m)
            Money.Create(200m)
            Money.Create(300m)
        ]
        let total = Money.sum moneys
        total.Amount |> should equal 600m

    [<Fact>]
    member _.``Money.fromDecimal と toDecimal で変換できる``() =
        let original = 12345m
        let money = Money.fromDecimal original
        let result = Money.toDecimal money
        result |> should equal original
