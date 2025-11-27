namespace FinancialAccounting.Domain.Events;

/// <summary>
/// 仕訳作成イベント（MassTransit で発行）
/// </summary>
public record JournalCreatedEvent(
    int JournalId,
    int FiscalYear,
    DateTime JournalDate,
    decimal TotalAmount
);
