using FinancialAccounting.Application.Ports.In;
using FinancialAccounting.Application.Ports.Out;
using FinancialAccounting.Domain.Entities;

namespace FinancialAccounting.Application.UseCases;

/// <summary>
/// 仕訳取得ユースケース実装
/// </summary>
public class GetJournalUseCase : IGetJournalUseCase
{
    private readonly IJournalRepository _journalRepository;

    public GetJournalUseCase(IJournalRepository journalRepository)
    {
        _journalRepository = journalRepository;
    }

    public async Task<Journal?> GetJournalByIdAsync(int journalId)
    {
        var journal = await _journalRepository.FindByIdAsync(journalId);
        if (journal == null) return null;

        var entries = await _journalRepository.FindEntriesByJournalIdAsync(journalId);
        journal.SetEntries(entries);

        return journal;
    }

    public async Task<List<Journal>> GetJournalsByFiscalYearAsync(int fiscalYear)
    {
        var journals = await _journalRepository.FindByFiscalYearAsync(fiscalYear);

        foreach (var journal in journals)
        {
            var entries = await _journalRepository.FindEntriesByJournalIdAsync(journal.JournalId!.Value);
            journal.SetEntries(entries);
        }

        return journals;
    }
}
