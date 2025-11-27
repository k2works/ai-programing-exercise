using FinancialAccounting.Domain.Entities;

namespace FinancialAccounting.Application.Ports.In;

/// <summary>
/// 仕訳作成ユースケース（入力ポート）
/// </summary>
public interface ICreateJournalUseCase
{
    Task<Journal> CreateJournalAsync(
        DateTime journalDate,
        string description,
        int fiscalYear,
        List<JournalEntryRequest> entries);
}

/// <summary>
/// 仕訳明細リクエスト
/// </summary>
public record JournalEntryRequest(
    string AccountCode,
    decimal DebitAmount,
    decimal CreditAmount,
    string? Description
);
