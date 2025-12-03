namespace AccountingSystem.Domain.Models

/// <summary>
/// 勘定科目構成エンティティ
/// </summary>
[<CLIMutable>]
type AccountStructure = {
    AccountCode: string                // 勘定科目コード
    AccountPath: string                // 勘定科目パス（チルダ連結）
    HierarchyLevel: int                // 階層レベル
    ParentAccountCode: string option   // 親科目コード
    DisplayOrder: int                  // 表示順序
}

/// AccountStructure エンティティのファクトリ関数とユーティリティ
module AccountStructure =
    let create accountCode accountPath hierarchyLevel displayOrder =
        {
            AccountCode = accountCode
            AccountPath = accountPath
            HierarchyLevel = hierarchyLevel
            ParentAccountCode = None
            DisplayOrder = displayOrder
        }

    /// <summary>
    /// パスから階層レベルを計算
    /// </summary>
    let calculateLevel (path: string) =
        path.Split('~').Length

    /// <summary>
    /// パスから親科目コードを取得
    /// </summary>
    let getParentCode (path: string) =
        let segments = path.Split('~')
        if segments.Length > 1 then
            Some segments.[segments.Length - 2]
        else
            None

    /// <summary>
    /// 子孫科目かどうかを判定
    /// </summary>
    let isDescendantOf (ancestorCode: string) (structure: AccountStructure) =
        structure.AccountPath.Contains($"~{ancestorCode}~") ||
        structure.AccountCode = ancestorCode
