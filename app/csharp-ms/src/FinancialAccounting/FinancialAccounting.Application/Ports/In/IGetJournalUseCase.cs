using FinancialAccounting.Domain.Entities;

namespace FinancialAccounting.Application.Ports.In;

/// <summary>
/// 仕訳取得ユースケース（入力ポート）
/// </summary>
public interface IGetJournalUseCase
{
    Task<Journal?> GetJournalByIdAsync(int journalId);
    Task<List<Journal>> GetJournalsByFiscalYearAsync(int fiscalYear);
}
