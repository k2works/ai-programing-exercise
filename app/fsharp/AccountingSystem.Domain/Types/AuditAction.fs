namespace AccountingSystem.Domain.Types

/// <summary>
/// 監査ログのアクション種別
/// </summary>
type AuditAction =
    | Create
    | Update
    | Delete

module AuditAction =
    /// コード値に変換
    let toCode (action: AuditAction) =
        match action with
        | Create -> "CREATE"
        | Update -> "UPDATE"
        | Delete -> "DELETE"

    /// コード値から変換
    let fromCode (code: string) =
        match code.ToUpper() with
        | "CREATE" -> Some Create
        | "UPDATE" -> Some Update
        | "DELETE" -> Some Delete
        | _ -> None

    /// 表示名を取得
    let getDisplayName (action: AuditAction) =
        match action with
        | Create -> "作成"
        | Update -> "更新"
        | Delete -> "削除"
