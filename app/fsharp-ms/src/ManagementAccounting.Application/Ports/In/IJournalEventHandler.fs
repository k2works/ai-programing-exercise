namespace ManagementAccounting.Application.Ports.In

open System.Threading.Tasks
open Shared.Contracts.Events

/// <summary>
/// 仕訳イベントハンドラインターフェース
/// 財務会計サービスからのイベントを処理する
/// </summary>
type IJournalEventHandler =
    /// <summary>
    /// 仕訳作成イベントを処理
    /// </summary>
    abstract member HandleJournalCreatedAsync: event: JournalCreatedEvent -> Task<unit>

    /// <summary>
    /// 仕訳更新イベントを処理
    /// </summary>
    abstract member HandleJournalUpdatedAsync: event: JournalUpdatedEvent -> Task<unit>

    /// <summary>
    /// 仕訳削除イベントを処理
    /// </summary>
    abstract member HandleJournalDeletedAsync: event: JournalDeletedEvent -> Task<unit>
