using FinancialAccounting.Domain.Entities;

namespace FinancialAccounting.Application.Ports.Out;

/// <summary>
/// 仕訳リポジトリ（出力ポート）
/// </summary>
public interface IJournalRepository
{
    Task<Journal> SaveAsync(Journal journal);
    Task<Journal?> FindByIdAsync(int journalId);
    Task<List<Journal>> FindByFiscalYearAsync(int fiscalYear);
    Task<List<JournalEntry>> FindEntriesByJournalIdAsync(int journalId);
}
