namespace FinancialAccounting.Domain.Entities;

/// <summary>
/// 仕訳エンティティ
/// </summary>
public class Journal
{
    public int? JournalId { get; set; }
    public DateTime JournalDate { get; set; }
    public string Description { get; set; } = string.Empty;
    public int FiscalYear { get; set; }
    public DateTime CreatedAt { get; set; }
    public DateTime UpdatedAt { get; set; }

    private readonly List<JournalEntry> _entries = new();
    public IReadOnlyList<JournalEntry> Entries => _entries.AsReadOnly();

    // Dapper 用のパラメータなしコンストラクタ
    public Journal() { }

    public Journal(DateTime journalDate, string description, int fiscalYear)
    {
        JournalDate = journalDate;
        Description = description;
        FiscalYear = fiscalYear;
    }

    /// <summary>
    /// 仕訳明細を追加（ビジネスルール: 借方・貸方の検証）
    /// </summary>
    public void AddEntry(JournalEntry entry)
    {
        ValidateEntry(entry);
        _entries.Add(entry);
    }

    /// <summary>
    /// 仕訳明細を一括設定（リポジトリからの復元用）
    /// </summary>
    public void SetEntries(IEnumerable<JournalEntry> entries)
    {
        _entries.Clear();
        _entries.AddRange(entries);
    }

    /// <summary>
    /// 借方合計と貸方合計が一致することを検証
    /// </summary>
    public void ValidateBalance()
    {
        var debitTotal = _entries.Sum(e => e.DebitAmount);
        var creditTotal = _entries.Sum(e => e.CreditAmount);

        if (debitTotal != creditTotal)
        {
            throw new InvalidOperationException(
                $"貸借が一致しません。借方合計: {debitTotal}, 貸方合計: {creditTotal}");
        }
    }

    private static void ValidateEntry(JournalEntry entry)
    {
        if (entry.DebitAmount == 0 && entry.CreditAmount == 0)
        {
            throw new ArgumentException("借方・貸方の少なくとも一方は0より大きい必要があります");
        }
    }
}

/// <summary>
/// 仕訳明細エンティティ
/// </summary>
public class JournalEntry
{
    public int? EntryId { get; set; }
    public int? JournalId { get; set; }
    public string AccountCode { get; set; } = string.Empty;
    public decimal DebitAmount { get; set; }
    public decimal CreditAmount { get; set; }
    public string? Description { get; set; }
}
