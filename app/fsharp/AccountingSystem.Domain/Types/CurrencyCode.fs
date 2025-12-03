namespace AccountingSystem.Domain.Types

open System

/// <summary>
/// 通貨コード（ISO 4217準拠）
/// </summary>
[<Struct>]
type CurrencyCode =
    private { Value: string }

    member this.Code = this.Value

    static member Create(code: string) =
        if String.IsNullOrWhiteSpace(code) then
            invalidArg "code" "通貨コードは空にできません"
        let trimmed = code.Trim().ToUpperInvariant()
        if trimmed.Length <> 3 then
            invalidArg "code" "通貨コードは3文字である必要があります（ISO 4217）"
        { Value = trimmed }

    static member JPY = CurrencyCode.Create("JPY")
    static member USD = CurrencyCode.Create("USD")
    static member EUR = CurrencyCode.Create("EUR")

module CurrencyCode =
    let value (code: CurrencyCode) = code.Code
    let equal (a: CurrencyCode) (b: CurrencyCode) = a.Code = b.Code
