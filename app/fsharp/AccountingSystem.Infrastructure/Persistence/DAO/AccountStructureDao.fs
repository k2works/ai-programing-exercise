namespace AccountingSystem.Infrastructure.Persistence.DAO

open AccountingSystem.Domain.Models
open AccountingSystem.Domain.Types

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
            AccountCode = AccountCode.Create(dao.AccountCode)
            AccountPath = dao.AccountPath
            HierarchyLevel = dao.HierarchyLevel
            ParentAccountCode = if isNull dao.ParentAccountCode then None else Some (AccountCode.Create(dao.ParentAccountCode))
            DisplayOrder = dao.DisplayOrder
        }

    /// ドメインモデルから DAO へ変換（INSERT/UPDATE 用パラメータ）
    let fromDomain (model: AccountStructure) =
        {|
            AccountCode = model.AccountCode.Code
            AccountPath = model.AccountPath
            HierarchyLevel = model.HierarchyLevel
            ParentAccountCode = model.ParentAccountCode |> Option.map (fun c -> c.Code) |> Option.toObj
            DisplayOrder = model.DisplayOrder
        |}
