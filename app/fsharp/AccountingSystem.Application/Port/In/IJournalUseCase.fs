namespace AccountingSystem.Application.Port.In

open System
open System.Threading.Tasks
open AccountingSystem.Domain.Models.Journal

/// <summary>
/// 仕訳ユースケースインターフェース
/// </summary>
type IJournalUseCase =
    /// <summary>
    /// 伝票番号で仕訳を取得
    /// </summary>
    /// <param name="voucherNumber">伝票番号</param>
    /// <returns>仕訳（見つからない場合は JournalNotFoundException）</returns>
    abstract member GetJournalByVoucherNumberAsync: voucherNumber:int -> Task<Journal>

    /// <summary>
    /// 日付範囲で仕訳を取得
    /// </summary>
    /// <param name="fromDate">開始日</param>
    /// <param name="toDate">終了日</param>
    /// <returns>仕訳リスト</returns>
    abstract member GetJournalsByDateRangeAsync: fromDate:DateTime * toDate:DateTime -> Task<Journal list>

    /// <summary>
    /// 新しい仕訳を作成
    /// </summary>
    /// <param name="journal">仕訳</param>
    /// <returns>作成された仕訳</returns>
    abstract member CreateJournalAsync: journal:Journal -> Task<Journal>

    /// <summary>
    /// 仕訳を更新
    /// </summary>
    /// <param name="voucherNumber">伝票番号</param>
    /// <param name="journal">更新する仕訳</param>
    /// <returns>更新された仕訳</returns>
    abstract member UpdateJournalAsync: voucherNumber:int -> journal:Journal -> Task<Journal>

    /// <summary>
    /// 仕訳を削除
    /// </summary>
    /// <param name="voucherNumber">伝票番号</param>
    abstract member DeleteJournalAsync: voucherNumber:int -> Task<unit>
