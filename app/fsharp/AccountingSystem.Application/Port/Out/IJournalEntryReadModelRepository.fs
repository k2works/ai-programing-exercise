namespace AccountingSystem.Application.Port.Out

open System
open System.Threading.Tasks

/// <summary>
/// 仕訳 Read Model エンティティ
/// </summary>
type JournalEntryReadModel = {
    Id: string
    EntryDate: DateTime
    Description: string
    Status: string
    Deleted: bool
    CreatedAt: DateTime
    UpdatedAt: DateTime
    ApprovedBy: string option
    ApprovalComment: string option
}

/// <summary>
/// 仕訳明細 Read Model エンティティ
/// </summary>
type JournalEntryLineReadModel = {
    Id: int64
    JournalEntryId: string
    AccountCode: string
    DebitCredit: string
    Amount: decimal
}

/// <summary>
/// 仕訳 Read Model リポジトリインターフェース（Port/Out）
/// </summary>
type IJournalEntryReadModelRepository =
    /// <summary>
    /// 仕訳を挿入
    /// </summary>
    abstract member InsertJournalEntryAsync:
        id: string ->
        entryDate: DateTime ->
        description: string ->
        status: string ->
        deleted: bool ->
        createdAt: DateTime ->
        updatedAt: DateTime ->
        approvedBy: string option ->
        approvalComment: string option ->
        Task<unit>

    /// <summary>
    /// 仕訳明細を挿入
    /// </summary>
    abstract member InsertJournalEntryLineAsync:
        journalEntryId: string ->
        accountCode: string ->
        debitCredit: string ->
        amount: decimal ->
        Task<unit>

    /// <summary>
    /// 仕訳ステータスを更新
    /// </summary>
    abstract member UpdateJournalEntryStatusAsync:
        id: string ->
        status: string ->
        updatedAt: DateTime ->
        approvedBy: string ->
        approvalComment: string ->
        Task<unit>

    /// <summary>
    /// 仕訳を削除済みにする
    /// </summary>
    abstract member MarkAsDeletedAsync:
        id: string ->
        updatedAt: DateTime ->
        Task<unit>

    /// <summary>
    /// ID で仕訳を取得
    /// </summary>
    abstract member SelectByIdAsync:
        id: string ->
        Task<JournalEntryReadModel option>

    /// <summary>
    /// 期間で仕訳を検索
    /// </summary>
    abstract member SelectByDateRangeAsync:
        startDate: DateTime ->
        endDate: DateTime ->
        Task<JournalEntryReadModel list>

    /// <summary>
    /// 仕訳明細を取得
    /// </summary>
    abstract member SelectLinesByJournalEntryIdAsync:
        journalEntryId: string ->
        Task<JournalEntryLineReadModel list>

    /// <summary>
    /// すべての仕訳を取得（削除済みを除く）
    /// </summary>
    abstract member SelectAllAsync:
        unit ->
        Task<JournalEntryReadModel list>
