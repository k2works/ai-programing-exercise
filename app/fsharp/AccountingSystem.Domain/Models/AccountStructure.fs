namespace AccountingSystem.Domain.Models

open AccountingSystem.Domain.Types

/// <summary>
/// 勘定科目構成エンティティ
/// </summary>
type AccountStructure = {
    AccountCode: AccountCode           // 勘定科目コード
    AccountPath: string                // 勘定科目パス（チルダ連結）
    HierarchyLevel: int                // 階層レベル
    ParentAccountCode: AccountCode option  // 親科目コード
    DisplayOrder: int                  // 表示順序
}

/// AccountStructure エンティティのファクトリ関数とユーティリティ
module AccountStructure =
    /// <summary>
    /// 新規 AccountStructure エンティティを作成
    /// </summary>
    let create accountCode accountPath hierarchyLevel displayOrder =
        {
            AccountCode = AccountCode.Create(accountCode)
            AccountPath = accountPath
            HierarchyLevel = hierarchyLevel
            ParentAccountCode = None
            DisplayOrder = displayOrder
        }

    /// <summary>
    /// エンティティ同一性の判定（AccountCode による識別）
    /// </summary>
    let equal (a: AccountStructure) (b: AccountStructure) =
        AccountCode.equal a.AccountCode b.AccountCode

    /// <summary>
    /// エンティティのハッシュコードを取得
    /// </summary>
    let hashCode (structure: AccountStructure) =
        structure.AccountCode.Code.GetHashCode()

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
    let isDescendantOf (ancestorCode: AccountCode) (structure: AccountStructure) =
        structure.AccountPath.Contains($"~{ancestorCode.Code}~") ||
        AccountCode.equal structure.AccountCode ancestorCode
