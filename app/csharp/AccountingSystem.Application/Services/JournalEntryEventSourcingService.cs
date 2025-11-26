namespace AccountingSystem.Application.Services;

using AccountingSystem.Application.Ports.In;
using AccountingSystem.Application.Ports.Out;
using AccountingSystem.Domain.Aggregates;

/// <summary>
/// 仕訳イベントソーシングサービス
/// </summary>
public class JournalEntryEventSourcingService : IJournalEntryEventSourcingService
{
    private readonly IEventStoreRepository _eventStoreRepository;

    public JournalEntryEventSourcingService(IEventStoreRepository eventStoreRepository)
    {
        _eventStoreRepository = eventStoreRepository;
    }

    /// <summary>
    /// 仕訳を作成
    /// </summary>
    public async Task<string> CreateJournalEntryAsync(
        DateOnly entryDate,
        string description,
        List<LineItemDto> lineItems,
        string userId)
    {
        var id = Guid.NewGuid().ToString();

        // Aggregate 作成（コマンド実行）
        var aggregate = JournalEntryAggregate.Create(
            id,
            entryDate,
            description,
            lineItems.Select(dto => new JournalEntryAggregate.LineItem(
                dto.AccountCode,
                Enum.Parse<JournalEntryAggregate.DebitCredit>(dto.DebitCredit),
                dto.Amount
            )).ToList(),
            userId
        );

        // イベントをイベントストアに保存
        await _eventStoreRepository.SaveAsync(
            id,
            aggregate.UncommittedEvents,
            0  // 新規作成なので expectedVersion = 0
        );

        aggregate.MarkEventsAsCommitted();

        return id;
    }

    /// <summary>
    /// 仕訳を承認
    /// </summary>
    public async Task ApproveJournalEntryAsync(string journalEntryId, string approvedBy, string comment)
    {
        // イベント再生で現在の状態を復元
        var events = await _eventStoreRepository.GetEventsAsync(journalEntryId);
        if (events.Count == 0)
        {
            throw new ArgumentException($"仕訳が見つかりません: {journalEntryId}");
        }

        var aggregate = JournalEntryAggregate.Replay(events);
        var currentVersion = aggregate.Version;

        // コマンド実行
        aggregate.Approve(approvedBy, comment);

        // 新しいイベントを保存
        await _eventStoreRepository.SaveAsync(
            journalEntryId,
            aggregate.UncommittedEvents,
            currentVersion
        );

        aggregate.MarkEventsAsCommitted();
    }

    /// <summary>
    /// 仕訳を削除
    /// </summary>
    public async Task DeleteJournalEntryAsync(string journalEntryId, string reason, string userId)
    {
        var events = await _eventStoreRepository.GetEventsAsync(journalEntryId);
        if (events.Count == 0)
        {
            throw new ArgumentException($"仕訳が見つかりません: {journalEntryId}");
        }

        var aggregate = JournalEntryAggregate.Replay(events);
        var currentVersion = aggregate.Version;

        aggregate.Delete(reason, userId);

        await _eventStoreRepository.SaveAsync(
            journalEntryId,
            aggregate.UncommittedEvents,
            currentVersion
        );

        aggregate.MarkEventsAsCommitted();
    }

    /// <summary>
    /// 仕訳を取得（イベント再生）
    /// </summary>
    public async Task<JournalEntryAggregate> GetJournalEntryAsync(string journalEntryId)
    {
        var events = await _eventStoreRepository.GetEventsAsync(journalEntryId);
        if (events.Count == 0)
        {
            throw new ArgumentException($"仕訳が見つかりません: {journalEntryId}");
        }
        return JournalEntryAggregate.Replay(events);
    }

    /// <summary>
    /// 特定時点の仕訳状態を取得（タイムトラベル）
    /// </summary>
    public async Task<JournalEntryAggregate> GetJournalEntryAtAsync(string journalEntryId, DateTime pointInTime)
    {
        var events = await _eventStoreRepository.GetEventsUntilAsync(journalEntryId, pointInTime);
        if (events.Count == 0)
        {
            throw new ArgumentException($"指定時点の仕訳が見つかりません: {journalEntryId}");
        }
        return JournalEntryAggregate.Replay(events);
    }
}
