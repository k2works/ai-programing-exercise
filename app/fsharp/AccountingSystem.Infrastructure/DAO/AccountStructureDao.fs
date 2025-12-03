namespace AccountingSystem.Infrastructure.DAO

open AccountingSystem.Domain.Models

/// <summary>
/// 勘定科目構成 DAO（Dapper マッピング用）
/// </summary>
[<CLIMutable>]
type AccountStructureDao = {
    AccountCode: string
    AccountPath: string
    HierarchyLevel: int
    ParentAccountCode: string
    DisplayOrder: int
}

module AccountStructureDao =
    /// DAO からドメインモデルへ変換
    let toDomain (dao: AccountStructureDao) : AccountStructure =
        {
            AccountCode = dao.AccountCode
            AccountPath = dao.AccountPath
            HierarchyLevel = dao.HierarchyLevel
            ParentAccountCode = if isNull dao.ParentAccountCode then None else Some dao.ParentAccountCode
            DisplayOrder = dao.DisplayOrder
        }

    /// ドメインモデルから DAO へ変換（INSERT/UPDATE 用パラメータ）
    let fromDomain (model: AccountStructure) =
        {|
            AccountCode = model.AccountCode
            AccountPath = model.AccountPath
            HierarchyLevel = model.HierarchyLevel
            ParentAccountCode = model.ParentAccountCode |> Option.toObj
            DisplayOrder = model.DisplayOrder
        |}
