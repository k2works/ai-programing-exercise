namespace AccountingSystem.Infrastructure.Entities;

/// <summary>
/// 仕訳貸借明細エンティティ
/// </summary>
public record JournalDetailItem
{
    /// <summary>仕訳伝票番号</summary>
    public required string JournalNo { get; init; }

    /// <summary>仕訳行番号</summary>
    public required int LineNumber { get; init; }

    /// <summary>仕訳行貸借区分（D=借方、C=貸方）</summary>
    public required string DebitCreditFlag { get; init; }

    /// <summary>通貨コード</summary>
    public required string CurrencyCode { get; init; }

    /// <summary>為替レート</summary>
    public required decimal ExchangeRate { get; init; }

    /// <summary>部門コード</summary>
    public string? DepartmentCode { get; init; }

    /// <summary>プロジェクトコード</summary>
    public string? ProjectCode { get; init; }

    /// <summary>勘定科目コード</summary>
    public required string AccountCode { get; init; }

    /// <summary>補助科目コード</summary>
    public string? SubAccountCode { get; init; }

    /// <summary>仕訳金額</summary>
    public required decimal Amount { get; init; }

    /// <summary>基軸換算仕訳金額</summary>
    public required decimal BaseAmount { get; init; }

    /// <summary>消費税区分</summary>
    public string? TaxType { get; init; }

    /// <summary>消費税率</summary>
    public int? TaxRate { get; init; }

    /// <summary>消費税計算区分</summary>
    public string? TaxCalcType { get; init; }

    /// <summary>期日</summary>
    public DateOnly? DueDate { get; init; }

    /// <summary>資金繰フラグ</summary>
    public required int CashFlowFlag { get; init; }

    /// <summary>セグメントコード</summary>
    public string? SegmentCode { get; init; }

    /// <summary>相手勘定科目コード</summary>
    public string? OffsetAccountCode { get; init; }

    /// <summary>相手補助科目コード</summary>
    public string? OffsetSubAccountCode { get; init; }

    /// <summary>付箋コード</summary>
    public string? NoteCode { get; init; }

    /// <summary>付箋内容</summary>
    public string? NoteContent { get; init; }

    /// <summary>作成日時</summary>
    public DateTime CreatedAt { get; init; }

    /// <summary>更新日時</summary>
    public DateTime UpdatedAt { get; init; }
}
