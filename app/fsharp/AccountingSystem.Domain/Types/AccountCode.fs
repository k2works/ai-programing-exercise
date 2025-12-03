namespace AccountingSystem.Domain.Types

open System

/// <summary>
/// 勘定科目コードを表す値オブジェクト
/// </summary>
[<Struct>]
type AccountCode =
    private { Value: string }

    /// <summary>
    /// コードの値を取得
    /// </summary>
    member this.Code = this.Value

    /// <summary>
    /// 勘定科目コードを作成
    /// </summary>
    static member Create(code: string) =
        if String.IsNullOrWhiteSpace(code) then
            invalidArg "code" "勘定科目コードは空にできません"
        { Value = code.Trim() }

    /// <summary>
    /// 勘定科目コードを作成（バリデーションなし）
    /// </summary>
    static member CreateUnsafe(code: string) =
        { Value = code }

    /// <summary>
    /// 指定した文字列で始まるかどうか
    /// </summary>
    member this.StartsWith(prefix: string) =
        this.Value.StartsWith(prefix, StringComparison.Ordinal)

    /// <summary>
    /// 指定した文字列を含むかどうか
    /// </summary>
    member this.Contains(value: string) =
        this.Value.Contains(value)

    /// <summary>
    /// 文字列表現
    /// </summary>
    override this.ToString() =
        this.Value

/// AccountCode モジュール
module AccountCode =
    /// <summary>
    /// string から AccountCode を作成
    /// </summary>
    let fromString (code: string) = AccountCode.Create(code)

    /// <summary>
    /// AccountCode から string を取得
    /// </summary>
    let toString (accountCode: AccountCode) = accountCode.Code

    /// <summary>
    /// 値オブジェクトの等価性判定
    /// </summary>
    let equal (a: AccountCode) (b: AccountCode) =
        a.Code = b.Code
