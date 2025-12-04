namespace AccountingSystem.Infrastructure.Persistence.DAO

open AccountingSystem.Domain.Models

/// <summary>
/// 課税取引 DAO（Dapper マッピング用）
/// </summary>
[<CLIMutable>]
type TaxTransactionDao = {
    TaxCode: string
    TaxName: string
    TaxRate: decimal
    Description: string
}

module TaxTransactionDao =
    /// DAO からドメインモデルへ変換
    let toDomain (dao: TaxTransactionDao) : TaxTransaction =
        {
            TaxCode = dao.TaxCode
            TaxName = dao.TaxName
            TaxRate = dao.TaxRate
            Description = if isNull dao.Description then None else Some dao.Description
        }

    /// ドメインモデルから DAO へ変換（INSERT/UPDATE 用パラメータ）
    let fromDomain (model: TaxTransaction) =
        {|
            TaxCode = model.TaxCode
            TaxName = model.TaxName
            TaxRate = model.TaxRate
            Description = model.Description |> Option.toObj
        |}
