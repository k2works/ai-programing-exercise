module AccountingSystem.Domain.Models.JournalLine

open System
open AccountingSystem.Domain.Types

/// <summary>
/// 仕訳明細エンティティ（3層構造の2層目）
/// 明細行の摘要を管理
/// </summary>
type JournalLine = {
    /// 仕訳伝票番号
    VoucherNumber: VoucherNumber
    /// 仕訳行番号
    LineNumber: int
    /// 行摘要
    Description: string
    /// 作成日時
    CreatedAt: DateTime
    /// 更新日時
    UpdatedAt: DateTime
}

module JournalLine =
    /// 仕訳明細を作成
    let create voucherNumber lineNumber description =
        {
            VoucherNumber = VoucherNumber.Create(voucherNumber)
            LineNumber = lineNumber
            Description = description
            CreatedAt = DateTime.UtcNow
            UpdatedAt = DateTime.UtcNow
        }

    /// エンティティの同一性判定（VoucherNumber + LineNumber で判定）
    let equal (a: JournalLine) (b: JournalLine) =
        a.VoucherNumber.Number = b.VoucherNumber.Number &&
        a.LineNumber = b.LineNumber

    /// ハッシュコード
    let hashCode (line: JournalLine) =
        hash (line.VoucherNumber.Number, line.LineNumber)
