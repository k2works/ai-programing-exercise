using AccountingSystem.Domain.Events;

namespace AccountingSystem.Application.Ports.In;

/// <summary>
/// 監査イベントリスナー Port（入力ポート）
/// ドメインイベントを受け取り、監査ログを記録する責務
/// </summary>
public interface IAuditEventHandler
{
    /// <summary>
    /// 勘定科目作成イベントを処理
    /// </summary>
    /// <param name="notification">勘定科目作成イベント</param>
    /// <param name="cancellationToken">キャンセルトークン</param>
    Task HandleAccountCreatedAsync(AccountCreatedEvent notification, CancellationToken cancellationToken = default);

    /// <summary>
    /// 勘定科目更新イベントを処理
    /// </summary>
    /// <param name="notification">勘定科目更新イベント</param>
    /// <param name="cancellationToken">キャンセルトークン</param>
    Task HandleAccountUpdatedAsync(AccountUpdatedEvent notification, CancellationToken cancellationToken = default);

    /// <summary>
    /// 勘定科目削除イベントを処理
    /// </summary>
    /// <param name="notification">勘定科目削除イベント</param>
    /// <param name="cancellationToken">キャンセルトークン</param>
    Task HandleAccountDeletedAsync(AccountDeletedEvent notification, CancellationToken cancellationToken = default);

    /// <summary>
    /// 仕訳作成イベントを処理
    /// </summary>
    /// <param name="notification">仕訳作成イベント</param>
    /// <param name="cancellationToken">キャンセルトークン</param>
    Task HandleJournalCreatedAsync(JournalCreatedEvent notification, CancellationToken cancellationToken = default);

    /// <summary>
    /// 仕訳削除イベントを処理
    /// </summary>
    /// <param name="notification">仕訳削除イベント</param>
    /// <param name="cancellationToken">キャンセルトークン</param>
    Task HandleJournalDeletedAsync(JournalDeletedEvent notification, CancellationToken cancellationToken = default);
}
