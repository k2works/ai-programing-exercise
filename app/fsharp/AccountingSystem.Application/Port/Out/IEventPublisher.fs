namespace AccountingSystem.Application.Port.Out

open System.Threading.Tasks
open AccountingSystem.Domain.Events

/// <summary>
/// イベントパブリッシャーインターフェース（Output Port）
/// ドメインイベントを発行するための抽象
/// </summary>
type IEventPublisher =
    /// <summary>
    /// 単一イベントを発行
    /// </summary>
    abstract member PublishAsync: event: JournalEntryEvent -> Task<unit>

    /// <summary>
    /// 複数イベントを発行
    /// </summary>
    abstract member PublishManyAsync: events: JournalEntryEvent list -> Task<unit>
