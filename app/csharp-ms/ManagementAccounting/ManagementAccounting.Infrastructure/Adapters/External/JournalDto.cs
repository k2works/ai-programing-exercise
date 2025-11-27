namespace ManagementAccounting.Infrastructure.Adapters.External;

/// <summary>
/// 財務会計サービスからの仕訳DTOレスポンス
/// </summary>
public class JournalDto
{
    public int JournalId { get; set; }
    public DateTime JournalDate { get; set; }
    public string Description { get; set; } = string.Empty;
    public int FiscalYear { get; set; }
    public List<JournalEntryDto> Entries { get; set; } = new();
}

/// <summary>
/// 仕訳明細DTOレスポンス
/// </summary>
public class JournalEntryDto
{
    public string AccountCode { get; set; } = string.Empty;
    public decimal DebitAmount { get; set; }
    public decimal CreditAmount { get; set; }
    public string Description { get; set; } = string.Empty;
}
