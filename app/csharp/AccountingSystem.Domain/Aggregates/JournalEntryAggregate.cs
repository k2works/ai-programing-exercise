namespace AccountingSystem.Domain.Aggregates;

using AccountingSystem.Domain.Events;

/// <summary>
/// 仕訳 Aggregate
/// イベントソーシングパターンを適用した仕訳の集約ルート
/// </summary>
public class JournalEntryAggregate
{
    public string Id { get; private set; } = string.Empty;
    public DateOnly EntryDate { get; private set; }
    public string Description { get; private set; } = string.Empty;
    public List<LineItem> LineItems { get; private set; } = new();
    public JournalEntryStatus Status { get; private set; } = JournalEntryStatus.DRAFT;
    public bool Deleted { get; private set; } = false;

    /// <summary>
    /// 未コミットイベント（まだイベントストアに保存されていない）
    /// </summary>
    private readonly List<IEventSourcedDomainEvent> _uncommittedEvents = new();
    public IReadOnlyList<IEventSourcedDomainEvent> UncommittedEvents => _uncommittedEvents.AsReadOnly();

    /// <summary>
    /// バージョン（楽観的ロック用）
    /// </summary>
    public int Version { get; private set; } = 0;

    /// <summary>
    /// イベント再生（Event Replay）
    /// 保存されたイベントから Aggregate の状態を復元
    /// </summary>
    public static JournalEntryAggregate Replay(IReadOnlyList<IEventSourcedDomainEvent> events)
    {
        var aggregate = new JournalEntryAggregate();
        foreach (var @event in events)
        {
            aggregate.Apply(@event);
            aggregate.Version++;
        }
        return aggregate;
    }

    /// <summary>
    /// コマンド: 仕訳を作成
    /// </summary>
    public static JournalEntryAggregate Create(
        string id,
        DateOnly entryDate,
        string description,
        List<LineItem> lineItems,
        string userId)
    {
        // ビジネスルール検証
        if (lineItems == null || lineItems.Count == 0)
        {
            throw new ArgumentException("仕訳明細が必要です");
        }

        var debitTotal = lineItems
            .Where(item => item.DebitCredit == DebitCredit.DEBIT)
            .Sum(item => item.Amount);

        var creditTotal = lineItems
            .Where(item => item.DebitCredit == DebitCredit.CREDIT)
            .Sum(item => item.Amount);

        if (debitTotal != creditTotal)
        {
            throw new ArgumentException(
                $"借方合計と貸方合計が一致しません: 借方={debitTotal}, 貸方={creditTotal}"
            );
        }

        // イベント発行
        var @event = new JournalEntryCreatedEvent
        {
            JournalEntryId = id,
            EntryDate = entryDate,
            Description = description,
            LineItems = lineItems.Select(item =>
                new JournalEntryCreatedEvent.JournalEntryLineItem
                {
                    AccountCode = item.AccountCode,
                    DebitCredit = (JournalEntryCreatedEvent.DebitCreditType)item.DebitCredit,
                    Amount = item.Amount
                }).ToList(),
            UserId = userId,
            OccurredAt = DateTime.UtcNow
        };

        var aggregate = new JournalEntryAggregate();
        aggregate.Apply(@event);
        aggregate._uncommittedEvents.Add(@event);

        return aggregate;
    }

    /// <summary>
    /// コマンド: 仕訳を承認
    /// </summary>
    public void Approve(string approvedBy, string approvalComment)
    {
        if (Deleted)
        {
            throw new InvalidOperationException("削除済みの仕訳は承認できません");
        }

        if (Status == JournalEntryStatus.APPROVED)
        {
            throw new InvalidOperationException("すでに承認済みです");
        }

        var @event = new JournalEntryApprovedEvent
        {
            JournalEntryId = Id,
            ApprovedBy = approvedBy,
            ApprovalComment = approvalComment,
            OccurredAt = DateTime.UtcNow,
            UserId = approvedBy
        };

        Apply(@event);
        _uncommittedEvents.Add(@event);
    }

    /// <summary>
    /// コマンド: 仕訳を削除
    /// </summary>
    public void Delete(string reason, string userId)
    {
        if (Deleted)
        {
            throw new InvalidOperationException("すでに削除済みです");
        }

        var @event = new JournalEntryDeletedEvent
        {
            JournalEntryId = Id,
            Reason = reason,
            OccurredAt = DateTime.UtcNow,
            UserId = userId
        };

        Apply(@event);
        _uncommittedEvents.Add(@event);
    }

    /// <summary>
    /// イベント適用（Apply）
    /// </summary>
    private void Apply(IEventSourcedDomainEvent @event)
    {
        switch (@event)
        {
            case JournalEntryCreatedEvent e:
                Id = e.JournalEntryId;
                EntryDate = e.EntryDate;
                Description = e.Description;
                LineItems = e.LineItems.Select(item => new LineItem(
                    item.AccountCode,
                    (DebitCredit)item.DebitCredit,
                    item.Amount
                )).ToList();
                Status = JournalEntryStatus.DRAFT;
                break;
            case JournalEntryApprovedEvent:
                Status = JournalEntryStatus.APPROVED;
                break;
            case JournalEntryDeletedEvent:
                Deleted = true;
                break;
        }
    }

    /// <summary>
    /// 未コミットイベントをクリア
    /// </summary>
    public void MarkEventsAsCommitted()
    {
        _uncommittedEvents.Clear();
    }

    /// <summary>
    /// 仕訳明細
    /// </summary>
    public class LineItem
    {
        public string AccountCode { get; }
        public DebitCredit DebitCredit { get; }
        public decimal Amount { get; }

        public LineItem(string accountCode, DebitCredit debitCredit, decimal amount)
        {
            AccountCode = accountCode;
            DebitCredit = debitCredit;
            Amount = amount;
        }
    }

    /// <summary>
    /// 借方・貸方区分
    /// </summary>
    public enum DebitCredit
    {
        DEBIT,
        CREDIT
    }

    /// <summary>
    /// 仕訳ステータス
    /// </summary>
    public enum JournalEntryStatus
    {
        DRAFT,
        APPROVED
    }
}
