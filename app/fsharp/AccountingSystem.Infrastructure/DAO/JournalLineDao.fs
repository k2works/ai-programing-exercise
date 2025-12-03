module AccountingSystem.Infrastructure.DAO.JournalLineDao

open System
open AccountingSystem.Domain.Models.JournalLine
open AccountingSystem.Domain.Types

/// <summary>
/// 仕訳明細テーブル（3層構造2層目）のDAO
/// </summary>
[<CLIMutable>]
type JournalLineDao = {
    VoucherNumber: string
    LineNumber: int16
    Description: string
    CreatedAt: DateTime
    UpdatedAt: DateTime
}

module JournalLineDao =
    /// ドメインモデルからDAOへ変換
    let fromDomain (line: JournalLine) : JournalLineDao =
        {
            VoucherNumber = line.VoucherNumber.Number
            LineNumber = int16 line.LineNumber
            Description = line.Description
            CreatedAt = line.CreatedAt
            UpdatedAt = line.UpdatedAt
        }

    /// DAOからドメインモデルへ変換
    let toDomain (dao: JournalLineDao) : JournalLine =
        {
            VoucherNumber = VoucherNumber.Create(dao.VoucherNumber)
            LineNumber = int dao.LineNumber
            Description = dao.Description
            CreatedAt = dao.CreatedAt
            UpdatedAt = dao.UpdatedAt
        }
