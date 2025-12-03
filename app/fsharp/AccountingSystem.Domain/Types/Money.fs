namespace AccountingSystem.Domain.Types

open System
open AccountingSystem.Domain.Types.Measure

/// <summary>
/// 金額を表す値オブジェクト（日本円単位）
/// F# の単位型（Units of Measure）を使用して型安全性を確保
/// </summary>
[<Struct>]
type Money =
    private { Value: decimal<円> }

    /// <summary>
    /// 金額の値を取得（decimal として）
    /// </summary>
    member this.Amount = removeUnit this.Value

    /// <summary>
    /// 金額の値を取得（単位付き）
    /// </summary>
    member this.Amount円 = this.Value

    /// <summary>
    /// ゼロ金額
    /// </summary>
    static member Zero = { Value = zero円 }

    /// <summary>
    /// 金額を作成（負の値も許可）
    /// </summary>
    static member Create(amount: decimal) =
        { Value = yen amount }

    /// <summary>
    /// 金額を作成（単位付き）
    /// </summary>
    static member Create円(amount: decimal<円>) =
        { Value = amount }

    /// <summary>
    /// 金額を作成（負の値は例外）
    /// </summary>
    static member CreatePositive(amount: decimal) =
        if amount < 0m then
            invalidArg "amount" "金額は0以上である必要があります"
        { Value = yen amount }

    /// <summary>
    /// 加算
    /// </summary>
    static member (+) (a: Money, b: Money) =
        { Value = a.Value + b.Value }

    /// <summary>
    /// 減算
    /// </summary>
    static member (-) (a: Money, b: Money) =
        { Value = a.Value - b.Value }

    /// <summary>
    /// スカラー乗算
    /// </summary>
    static member (*) (a: Money, multiplier: decimal) =
        { Value = a.Value * multiplier }

    /// <summary>
    /// スカラー除算
    /// </summary>
    static member (/) (a: Money, divisor: decimal) =
        if divisor = 0m then
            invalidArg "divisor" "ゼロで除算することはできません"
        { Value = a.Value / divisor }

    /// <summary>
    /// 絶対値を取得
    /// </summary>
    member this.Abs() =
        { Value = yen (Math.Abs(removeUnit this.Value)) }

    /// <summary>
    /// 負の金額かどうか
    /// </summary>
    member this.IsNegative = this.Value < zero円

    /// <summary>
    /// 正の金額かどうか
    /// </summary>
    member this.IsPositive = this.Value > zero円

    /// <summary>
    /// ゼロかどうか
    /// </summary>
    member this.IsZero = this.Value = zero円

    /// <summary>
    /// 符号を反転
    /// </summary>
    member this.Negate() =
        { Value = -this.Value }

    /// <summary>
    /// 指定した小数点以下桁数に丸める
    /// </summary>
    member this.Round(decimals: int) =
        { Value = yen (Math.Round(removeUnit this.Value, decimals, MidpointRounding.AwayFromZero)) }

    /// <summary>
    /// 文字列表現
    /// </summary>
    override this.ToString() =
        (removeUnit this.Value).ToString("N0")

    /// <summary>
    /// 通貨形式の文字列表現
    /// </summary>
    member this.ToFormattedString() =
        format円 this.Value

/// Money モジュール
module Money =
    /// <summary>
    /// decimal から Money を作成
    /// </summary>
    let fromDecimal (amount: decimal) = Money.Create(amount)

    /// <summary>
    /// Money から decimal を取得
    /// </summary>
    let toDecimal (money: Money) = money.Amount

    /// <summary>
    /// ゼロ金額
    /// </summary>
    let zero = Money.Zero

    /// <summary>
    /// 金額のリストを合計
    /// </summary>
    let sum (moneys: Money seq) =
        moneys |> Seq.fold (+) Money.Zero

    /// <summary>
    /// 値オブジェクトの等価性判定
    /// </summary>
    let equal (a: Money) (b: Money) =
        a.Amount = b.Amount
