using MediatR;

namespace AccountingSystem.Domain.Events;

/// <summary>
/// イベントソーシング用ドメインイベントインターフェース
/// </summary>
public interface IEventSourcedDomainEvent : INotification
{
    /// <summary>
    /// 集約ID
    /// </summary>
    string AggregateId { get; }

    /// <summary>
    /// イベント種別
    /// </summary>
    string EventType { get; }

    /// <summary>
    /// イベントバージョン（スキーマ進化対応）
    /// </summary>
    int EventVersion { get; }

    /// <summary>
    /// 発生日時
    /// </summary>
    DateTime OccurredAt { get; }

    /// <summary>
    /// ユーザーID
    /// </summary>
    string UserId { get; }
}
