using AccountingSystem.Application.Exceptions;
using AccountingSystem.Domain.Entities;
using AccountingSystem.Application.Ports.In;
using AccountingSystem.Application.Ports.Out;

namespace AccountingSystem.Application.Services;

/// <summary>
/// 仕訳サービス実装
/// </summary>
public class JournalService : IJournalService
{
    private readonly IJournalRepository _journalRepository;

    public JournalService(IJournalRepository journalRepository)
    {
        _journalRepository = journalRepository;
    }

    public async Task<Journal> GetJournalByNoAsync(string journalNo)
    {
        var journal = await _journalRepository.FindByJournalNoAsync(journalNo);

        if (journal == null)
        {
            throw new JournalNotFoundException($"仕訳伝票番号 {journalNo} が見つかりません");
        }

        return journal;
    }

    public async Task<Journal> CreateJournalAsync(Journal journal)
    {
        ValidateJournal(journal);

        // 既存チェック
        var existing = await _journalRepository.FindByJournalNoAsync(journal.JournalNo);
        if (existing != null)
        {
            throw new DuplicateJournalException($"仕訳伝票番号 {journal.JournalNo} は既に存在します");
        }

        await _journalRepository.InsertAsync(journal);

        return (await _journalRepository.FindByJournalNoAsync(journal.JournalNo))!;
    }

    public async Task DeleteJournalAsync(string journalNo)
    {
        var existing = await _journalRepository.FindByJournalNoAsync(journalNo);
        if (existing == null)
        {
            throw new JournalNotFoundException($"仕訳伝票番号 {journalNo} が見つかりません");
        }

        await _journalRepository.DeleteByJournalNoAsync(journalNo);
    }

    public async Task<(decimal DebitTotal, decimal CreditTotal, bool IsBalanced)> ValidateBalanceAsync(string journalNo)
    {
        var existing = await _journalRepository.FindByJournalNoAsync(journalNo);
        if (existing == null)
        {
            throw new JournalNotFoundException($"仕訳伝票番号 {journalNo} が見つかりません");
        }

        var (debitTotal, creditTotal) = await _journalRepository.GetBalanceAsync(journalNo);
        var isBalanced = debitTotal == creditTotal;

        return (debitTotal, creditTotal, isBalanced);
    }

    public async Task<IReadOnlyList<Journal>> GetJournalsByFiscalYearAsync(int fiscalYear)
    {
        return await _journalRepository.FindByFiscalYearAsync(fiscalYear);
    }

    private static void ValidateJournal(Journal journal)
    {
        if (string.IsNullOrWhiteSpace(journal.JournalNo))
        {
            throw new InvalidJournalEntryException("仕訳伝票番号は必須です");
        }

        if (journal.Details == null || journal.Details.Count == 0)
        {
            throw new InvalidJournalEntryException("仕訳明細は1件以上必要です");
        }

        // 複式簿記の検証: 借方・貸方の合計が一致するか
        decimal debitTotal = 0;
        decimal creditTotal = 0;

        foreach (var detail in journal.Details)
        {
            if (detail.Items == null || detail.Items.Count == 0)
            {
                throw new InvalidJournalEntryException($"行番号 {detail.LineNumber} の貸借明細は1件以上必要です");
            }

            foreach (var item in detail.Items)
            {
                if (string.IsNullOrWhiteSpace(item.AccountCode))
                {
                    throw new InvalidJournalEntryException($"行番号 {detail.LineNumber} の勘定科目コードは必須です");
                }

                if (item.Amount < 0)
                {
                    throw new InvalidJournalEntryException($"行番号 {detail.LineNumber} の金額は0以上である必要があります");
                }

                if (item.DebitCreditFlag == "D")
                {
                    debitTotal += item.Amount;
                }
                else if (item.DebitCreditFlag == "C")
                {
                    creditTotal += item.Amount;
                }
                else
                {
                    throw new InvalidJournalEntryException($"行番号 {detail.LineNumber} の貸借区分は 'D'（借方）または 'C'（貸方）である必要があります");
                }
            }
        }

        if (debitTotal != creditTotal)
        {
            throw new InvalidJournalEntryException(
                $"借方合計（{debitTotal:N0}）と貸方合計（{creditTotal:N0}）が一致しません。複式簿記の原則に違反しています。");
        }
    }
}
