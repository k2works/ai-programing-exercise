module AccountingSystem.Tests.Domain.MoneyTest

open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Types.Measure
open Xunit
open FsUnit.Xunit
open System

/// <summary>
/// Money 値オブジェクト（日本円単位）のテスト
/// </summary>
type MoneyTest() =

    [<Fact>]
    member _.``Money.Zero はゼロ円を返す``() =
        let zero = Money.Zero
        zero.Amount |> should equal 0m
        zero.Amount円 |> should equal 0m<円>

    [<Fact>]
    member _.``Money.Create で円金額を作成できる``() =
        let money = Money.Create(1000m)
        money.Amount |> should equal 1000m
        money.Amount円 |> should equal 1000m<円>

    [<Fact>]
    member _.``Money.Create円 で単位付き金額を作成できる``() =
        let money = Money.Create円(1000m<円>)
        money.Amount |> should equal 1000m
        money.Amount円 |> should equal 1000m<円>

    [<Fact>]
    member _.``Money.Create は負の円金額も許可する``() =
        let money = Money.Create(-500m)
        money.Amount |> should equal -500m
        money.Amount円 |> should equal -500m<円>

    [<Fact>]
    member _.``Money.CreatePositive は正の円金額を作成できる``() =
        let money = Money.CreatePositive(1000m)
        money.Amount |> should equal 1000m
        money.Amount円 |> should equal 1000m<円>

    [<Fact>]
    member _.``Money.CreatePositive は負の値で例外を投げる``() =
        (fun () -> Money.CreatePositive(-100m) |> ignore)
        |> should throw typeof<ArgumentException>

    [<Fact>]
    member _.``円金額の加算ができる``() =
        let a = Money.Create(100m)  // 100円
        let b = Money.Create(200m)  // 200円
        let result = a + b
        result.Amount |> should equal 300m
        result.Amount円 |> should equal 300m<円>

    [<Fact>]
    member _.``円金額の減算ができる``() =
        let a = Money.Create(500m)  // 500円
        let b = Money.Create(200m)  // 200円
        let result = a - b
        result.Amount |> should equal 300m
        result.Amount円 |> should equal 300m<円>

    [<Fact>]
    member _.``円金額のスカラー乗算ができる``() =
        let money = Money.Create(100m)  // 100円
        let result = money * 3m
        result.Amount |> should equal 300m
        result.Amount円 |> should equal 300m<円>

    [<Fact>]
    member _.``円金額のスカラー除算ができる``() =
        let money = Money.Create(300m)  // 300円
        let result = money / 3m
        result.Amount |> should equal 100m
        result.Amount円 |> should equal 100m<円>

    [<Fact>]
    member _.``円金額のゼロ除算は例外を投げる``() =
        let money = Money.Create(100m)  // 100円
        (fun () -> money / 0m |> ignore)
        |> should throw typeof<ArgumentException>

    [<Fact>]
    member _.``Money.Abs は円金額の絶対値を返す``() =
        let negative = Money.Create(-500m)  // -500円
        negative.Abs().Amount |> should equal 500m
        negative.Abs().Amount円 |> should equal 500m<円>

    [<Fact>]
    member _.``Money.IsNegative は負の円金額を判定する``() =
        let negative = Money.Create(-100m)  // -100円
        let positive = Money.Create(100m)   // 100円
        negative.IsNegative |> should equal true
        positive.IsNegative |> should equal false

    [<Fact>]
    member _.``Money.IsPositive は正の円金額を判定する``() =
        let positive = Money.Create(100m)   // 100円
        let negative = Money.Create(-100m)  // -100円
        positive.IsPositive |> should equal true
        negative.IsPositive |> should equal false

    [<Fact>]
    member _.``Money.IsZero はゼロ円を判定する``() =
        let zero = Money.Zero
        let nonZero = Money.Create(100m)  // 100円
        zero.IsZero |> should equal true
        nonZero.IsZero |> should equal false

    [<Fact>]
    member _.``Money.Negate は円金額の符号を反転する``() =
        let positive = Money.Create(100m)  // 100円
        let negative = positive.Negate()
        negative.Amount |> should equal -100m
        negative.Amount円 |> should equal -100m<円>

    [<Fact>]
    member _.``Money.Round は指定桁数に丸める``() =
        let money = Money.Create(123.456m)
        let rounded = money.Round(2)
        rounded.Amount |> should equal 123.46m

    [<Fact>]
    member _.``同じ円金額の Money は等しい``() =
        let a = Money.Create(1000m)  // 1000円
        let b = Money.Create(1000m)  // 1000円
        Money.equal a b |> should equal true

    [<Fact>]
    member _.``異なる円金額の Money は等しくない``() =
        let a = Money.Create(1000m)  // 1000円
        let b = Money.Create(2000m)  // 2000円
        Money.equal a b |> should equal false

    [<Fact>]
    member _.``Money.sum で円金額リストを合計できる``() =
        let moneys = [
            Money.Create(100m)   // 100円
            Money.Create(200m)   // 200円
            Money.Create(300m)   // 300円
        ]
        let total = Money.sum moneys
        total.Amount |> should equal 600m
        total.Amount円 |> should equal 600m<円>

    [<Fact>]
    member _.``Money.fromDecimal と toDecimal で変換できる``() =
        let original = 12345m
        let money = Money.fromDecimal original
        let result = Money.toDecimal money
        result |> should equal original

    [<Fact>]
    member _.``ToFormattedString で円記号付き文字列を取得できる``() =
        let money = Money.Create(10000m)  // 10000円
        money.ToFormattedString() |> should equal "¥10,000"

    [<Fact>]
    member _.``Measure.yen で単位付き金額を作成できる``() =
        let amount = yen 5000m
        amount |> should equal 5000m<円>

    [<Fact>]
    member _.``Measure.sum円 で単位付き金額を合計できる``() =
        let amounts = [100m<円>; 200m<円>; 300m<円>]
        let total = sum円 amounts
        total |> should equal 600m<円>

    [<Fact>]
    member _.``Measure.format円 で円記号付き文字列を取得できる``() =
        let amount = 10000m<円>
        format円 amount |> should equal "¥10,000"
