namespace AccountingSystem.Domain.Types

open System

/// <summary>
/// 伝票番号を表す値オブジェクト
/// </summary>
[<Struct>]
type VoucherNumber =
    private { Value: string }

    /// <summary>
    /// 伝票番号の値を取得
    /// </summary>
    member this.Number = this.Value

    /// <summary>
    /// 伝票番号を作成
    /// </summary>
    static member Create(number: string) =
        if String.IsNullOrWhiteSpace(number) then
            invalidArg "number" "伝票番号は空にできません"
        if number.Length > 10 then
            invalidArg "number" "伝票番号は10文字以内である必要があります"
        { Value = number.Trim() }

    /// <summary>
    /// 伝票番号を作成（バリデーションなし）
    /// </summary>
    static member CreateUnsafe(number: string) =
        { Value = number }

    /// <summary>
    /// 文字列表現
    /// </summary>
    override this.ToString() =
        this.Value

/// VoucherNumber モジュール
module VoucherNumber =
    /// <summary>
    /// string から VoucherNumber を作成
    /// </summary>
    let fromString (number: string) = VoucherNumber.Create(number)

    /// <summary>
    /// VoucherNumber から string を取得
    /// </summary>
    let toString (voucherNumber: VoucherNumber) = voucherNumber.Number

    /// <summary>
    /// 値オブジェクトの等価性判定
    /// </summary>
    let equal (a: VoucherNumber) (b: VoucherNumber) =
        a.Number = b.Number
