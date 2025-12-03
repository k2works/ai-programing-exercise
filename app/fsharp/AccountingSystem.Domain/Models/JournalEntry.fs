namespace AccountingSystem.Domain.Models

open System
open AccountingSystem.Domain.Types

/// <summary>
/// 仕訳エントリエンティティ（ヘッダー）
/// </summary>
type JournalEntry = {
    VoucherNumber: VoucherNumber       // 伝票番号
    EntryDate: DateTime                // 仕訳日
    Description: string                // 摘要
    TotalAmount: Money                 // 合計金額
    ReferenceNumber: string option     // 参照番号
    CreatedBy: string                  // 作成者
    CreatedAt: DateTime                // 作成日時
    UpdatedBy: string option           // 更新者
    UpdatedAt: DateTime                // 更新日時
}

/// JournalEntry エンティティのファクトリ関数とユーティリティ
module JournalEntry =
    /// <summary>
    /// 新規 JournalEntry エンティティを作成
    /// </summary>
    let create voucherNumber entryDate description totalAmount createdBy =
        {
            VoucherNumber = VoucherNumber.Create(voucherNumber)
            EntryDate = entryDate
            Description = description
            TotalAmount = totalAmount
            ReferenceNumber = None
            CreatedBy = createdBy
            CreatedAt = DateTime.UtcNow
            UpdatedBy = None
            UpdatedAt = DateTime.UtcNow
        }

    /// <summary>
    /// エンティティ同一性の判定（VoucherNumber による識別）
    /// </summary>
    let equal (a: JournalEntry) (b: JournalEntry) =
        VoucherNumber.equal a.VoucherNumber b.VoucherNumber

    /// <summary>
    /// エンティティのハッシュコードを取得
    /// </summary>
    let hashCode (entry: JournalEntry) =
        entry.VoucherNumber.Number.GetHashCode()
