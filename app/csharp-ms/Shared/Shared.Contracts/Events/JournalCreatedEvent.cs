namespace Shared.Contracts.Events;

/// <summary>
/// 仕訳作成イベント
///
/// 財務会計サービスで仕訳が作成されたときに発行され、
/// 管理会計サービスなど他のサービスで消費されます。
/// </summary>
public record JournalCreatedEvent(
    int JournalId,
    int FiscalYear,
    DateTime JournalDate,
    decimal TotalAmount
);
