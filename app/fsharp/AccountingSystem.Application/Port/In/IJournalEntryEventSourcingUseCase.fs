namespace AccountingSystem.Application.Port.In

open System
open System.Threading.Tasks
open AccountingSystem.Domain.Events
open AccountingSystem.Domain.Aggregates

/// <summary>
/// 仕訳イベントソーシング ユースケースインターフェース（Port/In）
/// </summary>
type IJournalEntryEventSourcingUseCase =
    /// <summary>
    /// 仕訳を作成
    /// </summary>
    abstract member CreateJournalEntryAsync:
        id: string ->
        entryDate: DateTime ->
        description: string ->
        lineItems: JournalEntryLineItem list ->
        userId: string ->
        Task<Result<JournalEntryAggregate, string>>

    /// <summary>
    /// 仕訳を承認
    /// </summary>
    abstract member ApproveJournalEntryAsync:
        id: string ->
        approvedBy: string ->
        approvalComment: string ->
        Task<Result<JournalEntryAggregate, string>>

    /// <summary>
    /// 仕訳を削除
    /// </summary>
    abstract member DeleteJournalEntryAsync:
        id: string ->
        reason: string ->
        userId: string ->
        Task<Result<JournalEntryAggregate, string>>

    /// <summary>
    /// 仕訳を取得（イベント再生）
    /// </summary>
    abstract member GetJournalEntryAsync:
        id: string ->
        Task<JournalEntryAggregate option>

    /// <summary>
    /// 特定時点の仕訳を取得（タイムトラベル）
    /// </summary>
    abstract member GetJournalEntryAtAsync:
        id: string ->
        pointInTime: DateTime ->
        Task<JournalEntryAggregate option>

    /// <summary>
    /// すべての仕訳を取得
    /// </summary>
    abstract member GetAllJournalEntriesAsync:
        unit ->
        Task<JournalEntryAggregate list>
