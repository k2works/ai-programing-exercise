module AccountingSystem.Infrastructure.DAO.JournalDao

open System
open AccountingSystem.Domain.Models.Journal
open AccountingSystem.Domain.Types

/// <summary>
/// 仕訳テーブル（3層構造ヘッダー）のDAO
/// </summary>
[<CLIMutable>]
type JournalDao = {
    VoucherNumber: string
    PostingDate: DateTime
    EntryDate: DateTime
    SettlementFlag: int16
    SingleEntryFlag: int16
    VoucherType: int16
    RecurringFlag: int16
    EmployeeCode: string
    DepartmentCode: string
    RedSlipFlag: int16
    RedBlackVoucherNumber: Nullable<int>
    CreatedAt: DateTime
    UpdatedAt: DateTime
}

module JournalDao =
    /// ドメインモデルからDAOへ変換
    let fromDomain (journal: Journal) : JournalDao =
        {
            VoucherNumber = journal.VoucherNumber.Number
            PostingDate = journal.PostingDate
            EntryDate = journal.EntryDate
            SettlementFlag = SettlementFlag.toInt journal.SettlementFlag |> int16
            SingleEntryFlag = if journal.IsSingleEntry then 1s else 0s
            VoucherType = VoucherType.toInt journal.VoucherType |> int16
            RecurringFlag = if journal.IsRecurring then 1s else 0s
            EmployeeCode = journal.EmployeeCode |> Option.defaultValue null
            DepartmentCode = journal.DepartmentCode |> Option.defaultValue null
            RedSlipFlag = RedSlipFlag.toInt journal.RedSlipFlag |> int16
            RedBlackVoucherNumber =
                journal.RedBlackVoucherNumber
                |> Option.map Nullable
                |> Option.defaultValue (Nullable())
            CreatedAt = DateTime.UtcNow
            UpdatedAt = DateTime.UtcNow
        }

    /// DAOからドメインモデルへ変換（Linesは空で返す、後からRepositoryで組み立て）
    let toDomain (dao: JournalDao) : Journal =
        {
            VoucherNumber = VoucherNumber.Create(dao.VoucherNumber)
            PostingDate = dao.PostingDate
            EntryDate = dao.EntryDate
            SettlementFlag =
                SettlementFlag.fromInt (int dao.SettlementFlag)
                |> Option.defaultValue SettlementFlag.Normal
            IsSingleEntry = dao.SingleEntryFlag = 1s
            VoucherType =
                VoucherType.fromInt (int dao.VoucherType)
                |> Option.defaultValue VoucherType.Transfer
            IsRecurring = dao.RecurringFlag = 1s
            EmployeeCode = if isNull dao.EmployeeCode then None else Some dao.EmployeeCode
            DepartmentCode = if isNull dao.DepartmentCode then None else Some dao.DepartmentCode
            RedSlipFlag =
                RedSlipFlag.fromInt (int dao.RedSlipFlag)
                |> Option.defaultValue RedSlipFlag.Normal
            RedBlackVoucherNumber =
                if dao.RedBlackVoucherNumber.HasValue then Some dao.RedBlackVoucherNumber.Value
                else None
            Lines = []
        }
