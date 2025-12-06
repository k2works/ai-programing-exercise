namespace AccountingSystem.Application.Port.In

open System.Threading.Tasks
open AccountingSystem.Domain.Events

/// <summary>
/// イベントハンドラーインターフェース（Input Port）
/// ドメインイベントを処理するための抽象
/// </summary>
type IEventHandler<'TEvent> =
    /// <summary>
    /// イベントを処理
    /// </summary>
    abstract member HandleAsync: event: 'TEvent -> Task<Result<unit, string>>

/// <summary>
/// 仕訳イベントハンドラーインターフェース
/// </summary>
type IJournalEntryEventHandler =
    inherit IEventHandler<JournalEntryEvent>

    /// <summary>
    /// ハンドラーが処理するイベントタイプ
    /// </summary>
    abstract member EventTypes: string list
