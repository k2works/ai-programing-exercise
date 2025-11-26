using System.ComponentModel.DataAnnotations;

namespace AccountingSystem.Infrastructure.Web.Dtos;

/// <summary>
/// 仕訳作成リクエスト DTO
/// </summary>
public record JournalRequest
{
    /// <summary>仕訳伝票番号</summary>
    [Required(ErrorMessage = "仕訳伝票番号は必須です")]
    [StringLength(20, ErrorMessage = "仕訳伝票番号は20文字以内で指定してください")]
    public required string JournalNo { get; init; }

    /// <summary>起票日（YYYY-MM-DD形式）</summary>
    [Required(ErrorMessage = "起票日は必須です")]
    public required DateOnly JournalDate { get; init; }

    /// <summary>入力日（YYYY-MM-DD形式）</summary>
    [Required(ErrorMessage = "入力日は必須です")]
    public required DateOnly InputDate { get; init; }

    /// <summary>決算仕訳フラグ（0=通常、1=決算）</summary>
    public int SettlementFlag { get; init; }

    /// <summary>単振フラグ（0=複合、1=単一）</summary>
    public int SingleEntryFlag { get; init; }

    /// <summary>仕訳伝票区分</summary>
    public int JournalType { get; init; }

    /// <summary>定期計上フラグ</summary>
    public int RecurringFlag { get; init; }

    /// <summary>社員コード</summary>
    [StringLength(10, ErrorMessage = "社員コードは10文字以内で指定してください")]
    public string? EmployeeCode { get; init; }

    /// <summary>部門コード</summary>
    [StringLength(10, ErrorMessage = "部門コードは10文字以内で指定してください")]
    public string? DepartmentCode { get; init; }

    /// <summary>赤伝フラグ（0=通常、1=赤伝）</summary>
    public int RedSlipFlag { get; init; }

    /// <summary>赤黒伝票番号</summary>
    [StringLength(20, ErrorMessage = "赤黒伝票番号は20文字以内で指定してください")]
    public string? RedBlackVoucherNo { get; init; }

    /// <summary>仕訳明細リスト</summary>
    [Required(ErrorMessage = "仕訳明細は必須です")]
    [MinLength(1, ErrorMessage = "仕訳明細は1件以上必要です")]
    public required List<JournalDetailRequest> Details { get; init; }
}

/// <summary>
/// 仕訳明細リクエスト DTO
/// </summary>
public record JournalDetailRequest
{
    /// <summary>仕訳行番号</summary>
    [Required(ErrorMessage = "仕訳行番号は必須です")]
    public required int LineNumber { get; init; }

    /// <summary>行摘要</summary>
    [Required(ErrorMessage = "行摘要は必須です")]
    [StringLength(200, ErrorMessage = "行摘要は200文字以内で指定してください")]
    public required string Description { get; init; }

    /// <summary>仕訳貸借明細リスト</summary>
    [Required(ErrorMessage = "仕訳貸借明細は必須です")]
    [MinLength(1, ErrorMessage = "仕訳貸借明細は1件以上必要です")]
    public required List<JournalDetailItemRequest> Items { get; init; }
}

/// <summary>
/// 仕訳貸借明細リクエスト DTO
/// </summary>
public record JournalDetailItemRequest
{
    /// <summary>仕訳行貸借区分（D=借方、C=貸方）</summary>
    [Required(ErrorMessage = "貸借区分は必須です")]
    [RegularExpression("^[DC]$", ErrorMessage = "貸借区分は 'D'（借方）または 'C'（貸方）で指定してください")]
    public required string DebitCreditFlag { get; init; }

    /// <summary>通貨コード</summary>
    [StringLength(3, ErrorMessage = "通貨コードは3文字以内で指定してください")]
    public string CurrencyCode { get; init; } = "JPY";

    /// <summary>為替レート</summary>
    public decimal ExchangeRate { get; init; } = 1m;

    /// <summary>部門コード</summary>
    [StringLength(10, ErrorMessage = "部門コードは10文字以内で指定してください")]
    public string? DepartmentCode { get; init; }

    /// <summary>プロジェクトコード</summary>
    [StringLength(10, ErrorMessage = "プロジェクトコードは10文字以内で指定してください")]
    public string? ProjectCode { get; init; }

    /// <summary>勘定科目コード</summary>
    [Required(ErrorMessage = "勘定科目コードは必須です")]
    [StringLength(10, ErrorMessage = "勘定科目コードは10文字以内で指定してください")]
    public required string AccountCode { get; init; }

    /// <summary>補助科目コード</summary>
    [StringLength(10, ErrorMessage = "補助科目コードは10文字以内で指定してください")]
    public string? SubAccountCode { get; init; }

    /// <summary>仕訳金額</summary>
    [Required(ErrorMessage = "仕訳金額は必須です")]
    [Range(0, double.MaxValue, ErrorMessage = "仕訳金額は0以上で指定してください")]
    public required decimal Amount { get; init; }

    /// <summary>基軸換算仕訳金額</summary>
    public decimal? BaseAmount { get; init; }

    /// <summary>消費税区分</summary>
    [StringLength(2, ErrorMessage = "消費税区分は2文字以内で指定してください")]
    public string? TaxType { get; init; }

    /// <summary>消費税率</summary>
    public int? TaxRate { get; init; }

    /// <summary>消費税計算区分</summary>
    [StringLength(1, ErrorMessage = "消費税計算区分は1文字で指定してください")]
    public string? TaxCalcType { get; init; }

    /// <summary>期日</summary>
    public DateOnly? DueDate { get; init; }

    /// <summary>資金繰フラグ</summary>
    public int CashFlowFlag { get; init; }

    /// <summary>セグメントコード</summary>
    [StringLength(10, ErrorMessage = "セグメントコードは10文字以内で指定してください")]
    public string? SegmentCode { get; init; }

    /// <summary>相手勘定科目コード</summary>
    [StringLength(10, ErrorMessage = "相手勘定科目コードは10文字以内で指定してください")]
    public string? OffsetAccountCode { get; init; }

    /// <summary>相手補助科目コード</summary>
    [StringLength(10, ErrorMessage = "相手補助科目コードは10文字以内で指定してください")]
    public string? OffsetSubAccountCode { get; init; }

    /// <summary>付箋コード</summary>
    [StringLength(10, ErrorMessage = "付箋コードは10文字以内で指定してください")]
    public string? NoteCode { get; init; }

    /// <summary>付箋内容</summary>
    [StringLength(200, ErrorMessage = "付箋内容は200文字以内で指定してください")]
    public string? NoteContent { get; init; }
}
