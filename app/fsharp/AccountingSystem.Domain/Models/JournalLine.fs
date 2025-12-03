module AccountingSystem.Domain.Models.JournalLine

open System
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Models.JournalLineItem

/// <summary>
/// 仕訳明細エンティティ（3層構造の2層目）
/// 明細行の摘要と貸借明細を管理
/// </summary>
type JournalLine = {
    /// 仕訳伝票番号
    VoucherNumber: VoucherNumber
    /// 仕訳行番号
    LineNumber: int
    /// 行摘要
    Description: string
    /// 仕訳貸借明細（3層目）
    Items: JournalLineItem list
}

module JournalLine =
    /// 仕訳明細を作成
    let create voucherNumber lineNumber description =
        {
            VoucherNumber = VoucherNumber.Create(voucherNumber)
            LineNumber = lineNumber
            Description = description
            Items = []
        }

    /// 仕訳明細を貸借明細付きで作成
    let createWithItems voucherNumber lineNumber description items =
        { create voucherNumber lineNumber description with Items = items }

    /// エンティティの同一性判定（VoucherNumber + LineNumber で判定）
    let equal (a: JournalLine) (b: JournalLine) =
        a.VoucherNumber.Number = b.VoucherNumber.Number &&
        a.LineNumber = b.LineNumber

    /// ハッシュコード
    let hashCode (line: JournalLine) =
        hash (line.VoucherNumber.Number, line.LineNumber)

    /// 借方合計を計算
    let sumDebit (line: JournalLine) =
        JournalLineItem.sumDebit line.Items

    /// 貸方合計を計算
    let sumCredit (line: JournalLine) =
        JournalLineItem.sumCredit line.Items

    /// 貸借バランスを検証
    let validateBalance (line: JournalLine) =
        JournalLineItem.validateBalance line.Items
