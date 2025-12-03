namespace AccountingSystem.Domain.Types

open System
open AccountingSystem.Domain.Types.Measure

/// <summary>
/// 通貨金額を表す値オブジェクト
/// 通貨コードと金額をペアで管理し、整合性を保証
/// </summary>
[<Struct>]
type CurrencyAmount =
    private {
        CurrencyValue: CurrencyCode
        AmountValue: decimal<Currency>
    }

    /// <summary>
    /// 通貨コードを取得
    /// </summary>
    member this.Currency = this.CurrencyValue

    /// <summary>
    /// 金額を取得（decimal として）
    /// </summary>
    member this.Amount = removeUnit this.AmountValue

    /// <summary>
    /// 金額を取得（単位付き）
    /// </summary>
    member this.AmountWithUnit = this.AmountValue

    /// <summary>
    /// ゼロ金額（日本円）
    /// </summary>
    static member ZeroJPY = { CurrencyValue = CurrencyCode.JPY; AmountValue = zeroCurrency }

    /// <summary>
    /// 通貨金額を作成
    /// </summary>
    static member Create(currencyCode: CurrencyCode, amount: decimal) =
        { CurrencyValue = currencyCode; AmountValue = currency amount }

    /// <summary>
    /// 日本円金額を作成
    /// </summary>
    static member CreateJPY(amount: decimal) =
        { CurrencyValue = CurrencyCode.JPY; AmountValue = currency amount }

    /// <summary>
    /// 日本円金額を作成（単位付き）
    /// </summary>
    static member CreateJPY円(amount: decimal<円>) =
        { CurrencyValue = CurrencyCode.JPY; AmountValue = currency (removeUnit amount) }

    /// <summary>
    /// 米ドル金額を作成
    /// </summary>
    static member CreateUSD(amount: decimal) =
        { CurrencyValue = CurrencyCode.USD; AmountValue = currency amount }

    /// <summary>
    /// ユーロ金額を作成
    /// </summary>
    static member CreateEUR(amount: decimal) =
        { CurrencyValue = CurrencyCode.EUR; AmountValue = currency amount }

    /// <summary>
    /// 同一通貨での加算
    /// </summary>
    static member (+) (a: CurrencyAmount, b: CurrencyAmount) =
        if not (CurrencyCode.equal a.CurrencyValue b.CurrencyValue) then
            invalidOp "異なる通貨間での加算はできません"
        { CurrencyValue = a.CurrencyValue; AmountValue = a.AmountValue + b.AmountValue }

    /// <summary>
    /// 同一通貨での減算
    /// </summary>
    static member (-) (a: CurrencyAmount, b: CurrencyAmount) =
        if not (CurrencyCode.equal a.CurrencyValue b.CurrencyValue) then
            invalidOp "異なる通貨間での減算はできません"
        { CurrencyValue = a.CurrencyValue; AmountValue = a.AmountValue - b.AmountValue }

    /// <summary>
    /// スカラー乗算
    /// </summary>
    static member (*) (a: CurrencyAmount, multiplier: decimal) =
        { CurrencyValue = a.CurrencyValue; AmountValue = a.AmountValue * multiplier }

    /// <summary>
    /// スカラー除算
    /// </summary>
    static member (/) (a: CurrencyAmount, divisor: decimal) =
        if divisor = 0m then
            invalidArg "divisor" "ゼロで除算することはできません"
        { CurrencyValue = a.CurrencyValue; AmountValue = a.AmountValue / divisor }

    /// <summary>
    /// 絶対値を取得
    /// </summary>
    member this.Abs() =
        { CurrencyValue = this.CurrencyValue; AmountValue = currency (Math.Abs(removeUnit this.AmountValue)) }

    /// <summary>
    /// 負の金額かどうか
    /// </summary>
    member this.IsNegative = this.AmountValue < zeroCurrency

    /// <summary>
    /// 正の金額かどうか
    /// </summary>
    member this.IsPositive = this.AmountValue > zeroCurrency

    /// <summary>
    /// ゼロかどうか
    /// </summary>
    member this.IsZero = this.AmountValue = zeroCurrency

    /// <summary>
    /// 符号を反転
    /// </summary>
    member this.Negate() =
        { CurrencyValue = this.CurrencyValue; AmountValue = -this.AmountValue }

    /// <summary>
    /// 日本円かどうか
    /// </summary>
    member this.IsJPY = CurrencyCode.equal this.CurrencyValue CurrencyCode.JPY

    /// <summary>
    /// Money型に変換（日本円の場合のみ有効）
    /// </summary>
    member this.ToMoney() =
        if not this.IsJPY then
            invalidOp "日本円以外はMoney型に変換できません"
        Money.Create(removeUnit this.AmountValue)

    /// <summary>
    /// 文字列表現
    /// </summary>
    override this.ToString() =
        sprintf "%s %s" this.CurrencyValue.Code ((removeUnit this.AmountValue).ToString("N2"))

/// CurrencyAmount モジュール
module CurrencyAmount =
    /// <summary>
    /// 値オブジェクトの等価性判定
    /// </summary>
    let equal (a: CurrencyAmount) (b: CurrencyAmount) =
        CurrencyCode.equal a.Currency b.Currency && a.Amount = b.Amount

    /// <summary>
    /// 同一通貨の金額リストを合計
    /// </summary>
    let sum (amounts: CurrencyAmount seq) =
        amounts |> Seq.fold (+) CurrencyAmount.ZeroJPY

    /// <summary>
    /// Money から CurrencyAmount を作成（日本円）
    /// </summary>
    let fromMoney (money: Money) =
        CurrencyAmount.CreateJPY(money.Amount)

    /// <summary>
    /// 為替レートを適用して円換算
    /// </summary>
    let toJPY (exchangeRate: decimal) (amount: CurrencyAmount) =
        Money.Create(amount.Amount * exchangeRate)
