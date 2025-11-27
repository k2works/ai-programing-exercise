using FinancialAccounting.Application.Ports.In;
using FinancialAccounting.Application.Ports.Out;
using FinancialAccounting.Domain.Entities;
using FinancialAccounting.Domain.Events;

namespace FinancialAccounting.Application.UseCases;

/// <summary>
/// 仕訳作成ユースケース実装
/// </summary>
public class CreateJournalUseCase : ICreateJournalUseCase
{
    private readonly IJournalRepository _journalRepository;
    private readonly IEventPublisher _eventPublisher;

    public CreateJournalUseCase(
        IJournalRepository journalRepository,
        IEventPublisher eventPublisher)
    {
        _journalRepository = journalRepository;
        _eventPublisher = eventPublisher;
    }

    public async Task<Journal> CreateJournalAsync(
        DateTime journalDate,
        string description,
        int fiscalYear,
        List<JournalEntryRequest> entries)
    {
        // 1. ドメインオブジェクトを生成
        var journal = new Journal(journalDate, description, fiscalYear);

        foreach (var entry in entries)
        {
            journal.AddEntry(new JournalEntry
            {
                AccountCode = entry.AccountCode,
                DebitAmount = entry.DebitAmount,
                CreditAmount = entry.CreditAmount,
                Description = entry.Description
            });
        }

        // 2. ビジネスルール検証（貸借一致）
        journal.ValidateBalance();

        // 3. 永続化
        var savedJournal = await _journalRepository.SaveAsync(journal);

        // 4. イベント発行
        var totalAmount = entries.Sum(e => e.DebitAmount);
        await _eventPublisher.PublishAsync(new JournalCreatedEvent(
            savedJournal.JournalId!.Value,
            savedJournal.FiscalYear,
            savedJournal.JournalDate,
            totalAmount
        ));

        return savedJournal;
    }
}
