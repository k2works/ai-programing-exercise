namespace AccountingSystem.Application.Repositories

open System.Threading.Tasks
open AccountingSystem.Domain.Models.AutoJournalPattern

/// <summary>
/// 自動仕訳パターン取得パラメータ
/// </summary>
type AutoJournalPatternQuery = {
    /// パターンコード（指定時は完全一致）
    PatternCode: string option
    /// 有効フラグ（指定時はフィルタリング）
    IsActive: bool option
}

/// <summary>
/// 自動仕訳実行結果
/// </summary>
type AutoJournalExecutionResult = {
    /// パターンID
    PatternId: int64
    /// 処理件数
    ProcessedCount: int
    /// 生成件数
    GeneratedCount: int
    /// ステータス
    Status: AutoJournalStatus
    /// メッセージ
    Message: string option
    /// エラー詳細
    ErrorDetail: string option
}

/// <summary>
/// 自動仕訳リポジトリインターフェース
/// Infrastructure 層で実装される
/// </summary>
type IAutoJournalRepository =
    /// <summary>
    /// パターン一覧を取得
    /// </summary>
    abstract member GetPatternsAsync: AutoJournalPatternQuery -> Task<AutoJournalPattern list>

    /// <summary>
    /// パターンをパターンコードで取得
    /// </summary>
    abstract member GetPatternByCodeAsync: patternCode: string -> Task<AutoJournalPattern option>

    /// <summary>
    /// パターン明細を取得
    /// </summary>
    abstract member GetPatternItemsAsync: patternId: int64 -> Task<AutoJournalPatternItem list>

    /// <summary>
    /// パターンを保存（新規作成または更新）
    /// </summary>
    abstract member SavePatternAsync: AutoJournalPattern -> Task<AutoJournalPattern>

    /// <summary>
    /// パターン明細を保存
    /// </summary>
    abstract member SavePatternItemsAsync: patternId: int64 -> items: AutoJournalPatternItem list -> Task<unit>

    /// <summary>
    /// 実行ログを保存
    /// </summary>
    abstract member SaveLogAsync: AutoJournalLog -> Task<AutoJournalLog>

    /// <summary>
    /// パターンの実行ログを取得
    /// </summary>
    abstract member GetLogsByPatternIdAsync: patternId: int64 -> limit: int -> Task<AutoJournalLog list>
