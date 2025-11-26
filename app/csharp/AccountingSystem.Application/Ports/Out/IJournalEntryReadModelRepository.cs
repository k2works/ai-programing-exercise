namespace AccountingSystem.Application.Ports.Out;

using AccountingSystem.Domain.Entities;

/// <summary>
/// 仕訳 Read Model リポジトリインターフェース
/// CQRS パターンの Query Side 用
/// </summary>
public interface IJournalEntryReadModelRepository
{
    /// <summary>
    /// 仕訳 Read Model を挿入
    /// </summary>
    Task InsertJournalEntryAsync(
        string id,
        DateOnly entryDate,
        string description,
        string status,
        bool deleted,
        DateTime createdAt,
        DateTime updatedAt,
        string? approvedBy,
        string? approvalComment);

    /// <summary>
    /// 仕訳明細 Read Model を挿入
    /// </summary>
    Task InsertJournalEntryLineAsync(
        string journalEntryId,
        string accountCode,
        string debitCredit,
        decimal amount);

    /// <summary>
    /// 仕訳ステータスを更新
    /// </summary>
    Task UpdateJournalEntryStatusAsync(
        string id,
        string status,
        DateTime updatedAt,
        string approvedBy,
        string approvalComment);

    /// <summary>
    /// 仕訳を論理削除
    /// </summary>
    Task MarkAsDeletedAsync(string id, DateTime updatedAt);

    /// <summary>
    /// 仕訳IDで取得
    /// </summary>
    Task<JournalEntryReadModel?> SelectByIdAsync(string id);

    /// <summary>
    /// 日付範囲で仕訳を取得
    /// </summary>
    Task<IReadOnlyList<JournalEntryReadModel>> SelectByDateRangeAsync(
        DateOnly startDate,
        DateOnly endDate);

    /// <summary>
    /// 仕訳IDで明細を取得
    /// </summary>
    Task<IReadOnlyList<JournalEntryLineReadModel>> SelectLinesByJournalEntryIdAsync(
        string journalEntryId);
}
