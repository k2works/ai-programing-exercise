namespace AccountingSystem.Application.Ports.In;

using AccountingSystem.Domain.Aggregates;

/// <summary>
/// 仕訳イベントソーシングサービスインターフェース
/// </summary>
public interface IJournalEntryEventSourcingService
{
    /// <summary>
    /// 仕訳を作成
    /// </summary>
    /// <param name="entryDate">仕訳日</param>
    /// <param name="description">摘要</param>
    /// <param name="lineItems">明細</param>
    /// <param name="userId">ユーザーID</param>
    /// <returns>作成された仕訳ID</returns>
    Task<string> CreateJournalEntryAsync(
        DateOnly entryDate,
        string description,
        List<LineItemDto> lineItems,
        string userId);

    /// <summary>
    /// 仕訳を承認
    /// </summary>
    /// <param name="journalEntryId">仕訳ID</param>
    /// <param name="approvedBy">承認者ID</param>
    /// <param name="comment">承認コメント</param>
    Task ApproveJournalEntryAsync(string journalEntryId, string approvedBy, string comment);

    /// <summary>
    /// 仕訳を削除
    /// </summary>
    /// <param name="journalEntryId">仕訳ID</param>
    /// <param name="reason">削除理由</param>
    /// <param name="userId">ユーザーID</param>
    Task DeleteJournalEntryAsync(string journalEntryId, string reason, string userId);

    /// <summary>
    /// 仕訳を取得（イベント再生）
    /// </summary>
    /// <param name="journalEntryId">仕訳ID</param>
    /// <returns>仕訳 Aggregate</returns>
    Task<JournalEntryAggregate> GetJournalEntryAsync(string journalEntryId);

    /// <summary>
    /// 特定時点の仕訳状態を取得（タイムトラベル）
    /// </summary>
    /// <param name="journalEntryId">仕訳ID</param>
    /// <param name="pointInTime">時点</param>
    /// <returns>指定時点の仕訳 Aggregate</returns>
    Task<JournalEntryAggregate> GetJournalEntryAtAsync(string journalEntryId, DateTime pointInTime);
}

/// <summary>
/// 仕訳明細 DTO
/// </summary>
public class LineItemDto
{
    /// <summary>
    /// 勘定科目コード
    /// </summary>
    public required string AccountCode { get; set; }

    /// <summary>
    /// 貸借区分（DEBIT, CREDIT）
    /// </summary>
    public required string DebitCredit { get; set; }

    /// <summary>
    /// 金額
    /// </summary>
    public required decimal Amount { get; set; }
}
