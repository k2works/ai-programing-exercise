namespace AccountingSystem.Domain.Entities;

/// <summary>
/// 仕訳 Read Model
/// CQRS パターンの Query Side 用エンティティ
/// </summary>
public class JournalEntryReadModel
{
    /// <summary>
    /// 仕訳ID
    /// </summary>
    public string Id { get; set; } = string.Empty;

    /// <summary>
    /// 仕訳日
    /// </summary>
    public DateOnly EntryDate { get; set; }

    /// <summary>
    /// 摘要
    /// </summary>
    public string Description { get; set; } = string.Empty;

    /// <summary>
    /// ステータス（DRAFT, APPROVED）
    /// </summary>
    public string Status { get; set; } = string.Empty;

    /// <summary>
    /// 論理削除フラグ
    /// </summary>
    public bool Deleted { get; set; }

    /// <summary>
    /// 作成日時
    /// </summary>
    public DateTime CreatedAt { get; set; }

    /// <summary>
    /// 更新日時
    /// </summary>
    public DateTime UpdatedAt { get; set; }

    /// <summary>
    /// 承認者ID
    /// </summary>
    public string? ApprovedBy { get; set; }

    /// <summary>
    /// 承認コメント
    /// </summary>
    public string? ApprovalComment { get; set; }

    /// <summary>
    /// 仕訳明細リスト（関連エンティティ）
    /// </summary>
    public List<JournalEntryLineReadModel> LineItems { get; set; } = new();
}

/// <summary>
/// 仕訳明細 Read Model
/// </summary>
public class JournalEntryLineReadModel
{
    /// <summary>
    /// 明細ID
    /// </summary>
    public long Id { get; set; }

    /// <summary>
    /// 仕訳ID
    /// </summary>
    public string JournalEntryId { get; set; } = string.Empty;

    /// <summary>
    /// 勘定科目コード
    /// </summary>
    public string AccountCode { get; set; } = string.Empty;

    /// <summary>
    /// 貸借区分（DEBIT, CREDIT）
    /// </summary>
    public string DebitCredit { get; set; } = string.Empty;

    /// <summary>
    /// 金額
    /// </summary>
    public decimal Amount { get; set; }
}
