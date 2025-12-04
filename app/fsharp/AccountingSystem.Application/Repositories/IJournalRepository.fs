namespace AccountingSystem.Application.Repositories

open System
open System.Threading.Tasks
open AccountingSystem.Domain.Models.Journal

/// <summary>
/// 仕訳リポジトリインターフェース（Output Port）
/// Infrastructure 層で実装される
/// </summary>
type IJournalRepository =
    /// <summary>
    /// 仕訳集約を登録
    /// </summary>
    /// <param name="journal">仕訳集約</param>
    /// <returns>伝票番号</returns>
    abstract member SaveAsync: journal:Journal -> Task<string>

    /// <summary>
    /// 伝票番号で仕訳を検索
    /// </summary>
    /// <param name="voucherNumber">伝票番号</param>
    /// <returns>仕訳集約（存在しない場合は None）</returns>
    abstract member FindByVoucherNumberAsync: voucherNumber:string -> Task<Journal option>

    /// <summary>
    /// 起票日範囲で仕訳を検索
    /// </summary>
    /// <param name="fromDate">開始日</param>
    /// <param name="toDate">終了日</param>
    /// <returns>仕訳リスト（ヘッダーのみ）</returns>
    abstract member FindByDateRangeAsync: fromDate:DateTime * toDate:DateTime -> Task<Journal list>

    /// <summary>
    /// 仕訳を更新
    /// </summary>
    /// <param name="journal">仕訳</param>
    /// <returns>更新された行数</returns>
    abstract member UpdateAsync: journal:Journal -> Task<int>

    /// <summary>
    /// 仕訳を削除
    /// </summary>
    /// <param name="voucherNumber">伝票番号</param>
    /// <returns>削除された行数</returns>
    abstract member DeleteAsync: voucherNumber:string -> Task<int>
