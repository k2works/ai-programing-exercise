namespace AccountingSystem.Infrastructure.DAO

open AccountingSystem.Domain.Models
open AccountingSystem.Domain.Types

/// <summary>
/// 仕訳明細 DAO（Dapper マッピング用）
/// </summary>
[<CLIMutable>]
type JournalDetailDao = {
    VoucherNumber: string
    LineNumber: int
    AccountCode: string
    DebitAmount: decimal
    CreditAmount: decimal
    Description: string
    TaxAmount: System.Nullable<decimal>
    TaxRate: System.Nullable<decimal>
}

module JournalDetailDao =
    /// DAO からドメインモデルへ変換
    let toDomain (dao: JournalDetailDao) : JournalDetail =
        {
            VoucherNumber = VoucherNumber.Create(dao.VoucherNumber)
            LineNumber = dao.LineNumber
            AccountCode = AccountCode.Create(dao.AccountCode)
            DebitAmount = Money.Create(dao.DebitAmount)
            CreditAmount = Money.Create(dao.CreditAmount)
            Description = dao.Description
            TaxAmount = if dao.TaxAmount.HasValue then Some (Money.Create(dao.TaxAmount.Value)) else None
            TaxRate = if dao.TaxRate.HasValue then Some dao.TaxRate.Value else None
        }

    /// ドメインモデルから DAO へ変換（INSERT/UPDATE 用パラメータ）
    let fromDomain (model: JournalDetail) =
        {|
            VoucherNumber = model.VoucherNumber.Number
            LineNumber = model.LineNumber
            AccountCode = model.AccountCode.Code
            DebitAmount = model.DebitAmount.Amount
            CreditAmount = model.CreditAmount.Amount
            Description = model.Description
            TaxAmount = model.TaxAmount |> Option.map (fun m -> m.Amount) |> Option.toNullable
            TaxRate = model.TaxRate |> Option.toNullable
        |}
