namespace AccountingSystem.Infrastructure.DAO

open System
open AccountingSystem.Domain.Models
open AccountingSystem.Domain.Types

/// <summary>
/// 仕訳エントリ DAO（Dapper マッピング用）
/// </summary>
[<CLIMutable>]
type JournalEntryDao = {
    VoucherNumber: string
    EntryDate: DateTime
    Description: string
    TotalAmount: decimal
    ReferenceNumber: string
    CreatedBy: string
    CreatedAt: DateTime
    UpdatedBy: string
    UpdatedAt: DateTime
}

module JournalEntryDao =
    /// DAO からドメインモデルへ変換
    let toDomain (dao: JournalEntryDao) : JournalEntry =
        {
            VoucherNumber = VoucherNumber.Create(dao.VoucherNumber)
            EntryDate = dao.EntryDate
            Description = dao.Description
            TotalAmount = Money.Create(dao.TotalAmount)
            ReferenceNumber = if isNull dao.ReferenceNumber then None else Some dao.ReferenceNumber
            CreatedBy = dao.CreatedBy
            CreatedAt = dao.CreatedAt
            UpdatedBy = if isNull dao.UpdatedBy then None else Some dao.UpdatedBy
            UpdatedAt = dao.UpdatedAt
        }

    /// ドメインモデルから DAO へ変換（INSERT/UPDATE 用パラメータ）
    let fromDomain (model: JournalEntry) =
        {|
            VoucherNumber = model.VoucherNumber.Number
            EntryDate = model.EntryDate
            Description = model.Description
            TotalAmount = model.TotalAmount.Amount
            ReferenceNumber = model.ReferenceNumber |> Option.toObj
            CreatedBy = model.CreatedBy
            UpdatedBy = model.UpdatedBy |> Option.toObj
        |}
