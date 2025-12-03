namespace AccountingSystem.Domain.Types

/// <summary>
/// 単位型（Units of Measure）の定義
/// F# の単位型機能を使用して型安全な数値計算を実現
/// </summary>
module Measure =
    /// <summary>
    /// 日本円の単位型
    /// </summary>
    [<Measure>]
    type 円

    /// <summary>
    /// 米ドルの単位型
    /// </summary>
    [<Measure>]
    type USD

    /// <summary>
    /// ユーロの単位型
    /// </summary>
    [<Measure>]
    type EUR

    /// <summary>
    /// 汎用通貨単位（通貨コードで区別する場合に使用）
    /// </summary>
    [<Measure>]
    type Currency

    /// <summary>
    /// 日本円の金額を作成
    /// </summary>
    let yen (amount: decimal) : decimal<円> = LanguagePrimitives.DecimalWithMeasure amount

    /// <summary>
    /// 米ドルの金額を作成
    /// </summary>
    let usd (amount: decimal) : decimal<USD> = LanguagePrimitives.DecimalWithMeasure amount

    /// <summary>
    /// ユーロの金額を作成
    /// </summary>
    let eur (amount: decimal) : decimal<EUR> = LanguagePrimitives.DecimalWithMeasure amount

    /// <summary>
    /// 汎用通貨金額を作成
    /// </summary>
    let currency (amount: decimal) : decimal<Currency> = LanguagePrimitives.DecimalWithMeasure amount

    /// <summary>
    /// 単位を除去して decimal を取得
    /// </summary>
    let inline removeUnit (x: decimal<'u>) : decimal = decimal x

    /// <summary>
    /// ゼロ円
    /// </summary>
    let zero円 : decimal<円> = 0m<円>

    /// <summary>
    /// ゼロ（汎用通貨）
    /// </summary>
    let zeroCurrency : decimal<Currency> = 0m<Currency>

    /// <summary>
    /// 円の金額を合計
    /// </summary>
    let sum円 (amounts: decimal<円> seq) : decimal<円> =
        amounts |> Seq.fold (+) zero円

    /// <summary>
    /// 汎用通貨金額を合計
    /// </summary>
    let sumCurrency (amounts: decimal<Currency> seq) : decimal<Currency> =
        amounts |> Seq.fold (+) zeroCurrency

    /// <summary>
    /// 円の金額を文字列に変換
    /// </summary>
    let format円 (amount: decimal<円>) : string =
        sprintf "¥%s" ((removeUnit amount).ToString("N0"))
