namespace FinancialAccounting.Application.Ports.Out

open System.Threading.Tasks
open FinancialAccounting.Domain.Models

/// <summary>
/// 仕訳イベントの発行者インターフェース（出力ポート）
/// </summary>
type IJournalEventPublisher =
    /// <summary>
    /// 仕訳作成イベントを発行
    /// </summary>
    abstract member PublishJournalCreatedAsync: Journal -> Task

    /// <summary>
    /// 仕訳更新イベントを発行
    /// </summary>
    abstract member PublishJournalUpdatedAsync: Journal -> Task

    /// <summary>
    /// 仕訳削除イベントを発行
    /// journalId: 削除された仕訳ID
    /// fiscalYear: 会計年度
    /// </summary>
    abstract member PublishJournalDeletedAsync: journalId: int * fiscalYear: int -> Task
