using AccountingSystem.Domain.Entities;

namespace AccountingSystem.Infrastructure.Web.Dtos;

/// <summary>
/// 仕訳レスポンス DTO
/// </summary>
public record JournalResponse
{
    /// <summary>仕訳伝票番号</summary>
    public required string JournalNo { get; init; }

    /// <summary>起票日</summary>
    public required DateOnly JournalDate { get; init; }

    /// <summary>入力日</summary>
    public required DateOnly InputDate { get; init; }

    /// <summary>決算仕訳フラグ</summary>
    public int SettlementFlag { get; init; }

    /// <summary>単振フラグ</summary>
    public int SingleEntryFlag { get; init; }

    /// <summary>仕訳伝票区分</summary>
    public int JournalType { get; init; }

    /// <summary>定期計上フラグ</summary>
    public int RecurringFlag { get; init; }

    /// <summary>社員コード</summary>
    public string? EmployeeCode { get; init; }

    /// <summary>部門コード</summary>
    public string? DepartmentCode { get; init; }

    /// <summary>赤伝フラグ</summary>
    public int RedSlipFlag { get; init; }

    /// <summary>赤黒伝票番号</summary>
    public string? RedBlackVoucherNo { get; init; }

    /// <summary>仕訳明細リスト</summary>
    public required List<JournalDetailResponse> Details { get; init; }

    /// <summary>Entity から DTO を生成</summary>
    public static JournalResponse From(Journal journal) => new()
    {
        JournalNo = journal.JournalNo,
        JournalDate = journal.JournalDate,
        InputDate = journal.InputDate,
        SettlementFlag = journal.SettlementFlag,
        SingleEntryFlag = journal.SingleEntryFlag,
        JournalType = journal.JournalType,
        RecurringFlag = journal.RecurringFlag,
        EmployeeCode = journal.EmployeeCode,
        DepartmentCode = journal.DepartmentCode,
        RedSlipFlag = journal.RedSlipFlag,
        RedBlackVoucherNo = journal.RedBlackVoucherNo,
        Details = journal.Details.Select(JournalDetailResponse.From).ToList()
    };

    /// <summary>DTO から Entity を生成</summary>
    public static Journal ToEntity(JournalRequest request) => new()
    {
        JournalNo = request.JournalNo,
        JournalDate = request.JournalDate,
        InputDate = request.InputDate,
        SettlementFlag = request.SettlementFlag,
        SingleEntryFlag = request.SingleEntryFlag,
        JournalType = request.JournalType,
        RecurringFlag = request.RecurringFlag,
        EmployeeCode = request.EmployeeCode,
        DepartmentCode = request.DepartmentCode,
        RedSlipFlag = request.RedSlipFlag,
        RedBlackVoucherNo = request.RedBlackVoucherNo,
        Details = request.Details.Select((d, i) => JournalDetailResponse.ToEntity(request.JournalNo, d)).ToList()
    };
}

/// <summary>
/// 仕訳明細レスポンス DTO
/// </summary>
public record JournalDetailResponse
{
    /// <summary>仕訳行番号</summary>
    public required int LineNumber { get; init; }

    /// <summary>行摘要</summary>
    public required string Description { get; init; }

    /// <summary>仕訳貸借明細リスト</summary>
    public required List<JournalDetailItemResponse> Items { get; init; }

    /// <summary>Entity から DTO を生成</summary>
    public static JournalDetailResponse From(JournalDetail detail) => new()
    {
        LineNumber = detail.LineNumber,
        Description = detail.Description,
        Items = detail.Items.Select(JournalDetailItemResponse.From).ToList()
    };

    /// <summary>DTO から Entity を生成</summary>
    public static JournalDetail ToEntity(string journalNo, JournalDetailRequest request) => new()
    {
        JournalNo = journalNo,
        LineNumber = request.LineNumber,
        Description = request.Description,
        Items = request.Items.Select(i => JournalDetailItemResponse.ToEntity(journalNo, request.LineNumber, i)).ToList()
    };
}

/// <summary>
/// 仕訳貸借明細レスポンス DTO
/// </summary>
public record JournalDetailItemResponse
{
    /// <summary>仕訳行貸借区分</summary>
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
    public int CashFlowFlag { get; init; }

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

    /// <summary>Entity から DTO を生成</summary>
    public static JournalDetailItemResponse From(JournalDetailItem item) => new()
    {
        DebitCreditFlag = item.DebitCreditFlag,
        CurrencyCode = item.CurrencyCode,
        ExchangeRate = item.ExchangeRate,
        DepartmentCode = item.DepartmentCode,
        ProjectCode = item.ProjectCode,
        AccountCode = item.AccountCode,
        SubAccountCode = item.SubAccountCode,
        Amount = item.Amount,
        BaseAmount = item.BaseAmount,
        TaxType = item.TaxType,
        TaxRate = item.TaxRate,
        TaxCalcType = item.TaxCalcType,
        DueDate = item.DueDate,
        CashFlowFlag = item.CashFlowFlag,
        SegmentCode = item.SegmentCode,
        OffsetAccountCode = item.OffsetAccountCode,
        OffsetSubAccountCode = item.OffsetSubAccountCode,
        NoteCode = item.NoteCode,
        NoteContent = item.NoteContent
    };

    /// <summary>DTO から Entity を生成</summary>
    public static JournalDetailItem ToEntity(string journalNo, int lineNumber, JournalDetailItemRequest request) => new()
    {
        JournalNo = journalNo,
        LineNumber = lineNumber,
        DebitCreditFlag = request.DebitCreditFlag,
        CurrencyCode = request.CurrencyCode,
        ExchangeRate = request.ExchangeRate,
        DepartmentCode = request.DepartmentCode,
        ProjectCode = request.ProjectCode,
        AccountCode = request.AccountCode,
        SubAccountCode = request.SubAccountCode,
        Amount = request.Amount,
        BaseAmount = request.BaseAmount ?? request.Amount,
        TaxType = request.TaxType,
        TaxRate = request.TaxRate,
        TaxCalcType = request.TaxCalcType,
        DueDate = request.DueDate,
        CashFlowFlag = request.CashFlowFlag,
        SegmentCode = request.SegmentCode,
        OffsetAccountCode = request.OffsetAccountCode,
        OffsetSubAccountCode = request.OffsetSubAccountCode,
        NoteCode = request.NoteCode,
        NoteContent = request.NoteContent
    };
}

/// <summary>
/// 仕訳残高検証レスポンス DTO
/// </summary>
public record JournalBalanceResponse
{
    /// <summary>仕訳伝票番号</summary>
    public required string JournalNo { get; init; }

    /// <summary>借方合計</summary>
    public required decimal DebitTotal { get; init; }

    /// <summary>貸方合計</summary>
    public required decimal CreditTotal { get; init; }

    /// <summary>貸借バランスが取れているか</summary>
    public required bool IsBalanced { get; init; }
}

/// <summary>
/// 仕訳一覧サマリーレスポンス DTO（Script Lab 用）
/// </summary>
public record JournalSummaryResponse
{
    /// <summary>仕訳伝票番号</summary>
    public required string JournalNo { get; init; }

    /// <summary>起票日</summary>
    public required DateOnly JournalDate { get; init; }

    /// <summary>決算期（年度）</summary>
    public required int FiscalYear { get; init; }

    /// <summary>摘要（最初の明細の行摘要）</summary>
    public string? Description { get; init; }

    /// <summary>借方合計</summary>
    public required decimal TotalDebit { get; init; }

    /// <summary>貸方合計</summary>
    public required decimal TotalCredit { get; init; }

    /// <summary>Entity から DTO を生成</summary>
    public static JournalSummaryResponse From(Journal journal)
    {
        // 日本の会計年度: 4月〜翌年3月
        var fiscalYear = journal.JournalDate.Month >= 4
            ? journal.JournalDate.Year
            : journal.JournalDate.Year - 1;

        var debitTotal = journal.Details
            .SelectMany(d => d.Items)
            .Where(i => i.DebitCreditFlag == "D")
            .Sum(i => i.Amount);

        var creditTotal = journal.Details
            .SelectMany(d => d.Items)
            .Where(i => i.DebitCreditFlag == "C")
            .Sum(i => i.Amount);

        return new JournalSummaryResponse
        {
            JournalNo = journal.JournalNo,
            JournalDate = journal.JournalDate,
            FiscalYear = fiscalYear,
            Description = journal.Details.FirstOrDefault()?.Description,
            TotalDebit = debitTotal,
            TotalCredit = creditTotal
        };
    }
}
