module AccountingSystem.Domain.Models.AutoJournalPattern

open System
open AccountingSystem.Domain.Types

/// <summary>
/// 自動仕訳パターンエンティティ
/// 仕訳生成ルールを定義
/// </summary>
type AutoJournalPattern = {
    /// パターンID
    Id: int64
    /// パターンコード（エンティティID）
    PatternCode: string
    /// パターン名
    PatternName: string
    /// ソーステーブル名
    SourceTableName: string
    /// 説明
    Description: string option
    /// 有効フラグ
    IsActive: bool
}

/// <summary>
/// 自動仕訳パターン明細エンティティ
/// パターンに紐づく仕訳生成ルールの詳細
/// </summary>
type AutoJournalPatternItem = {
    /// 明細ID
    Id: int64
    /// パターンID
    PatternId: int64
    /// 行番号
    LineNumber: int
    /// 貸借区分
    DebitCreditType: DebitCreditType
    /// 勘定科目コード
    AccountCode: AccountCode
    /// 金額式（SQL式やプレースホルダー）
    AmountExpression: string
    /// 摘要テンプレート
    DescriptionTemplate: string option
}

/// <summary>
/// 自動仕訳実行ステータス
/// </summary>
type AutoJournalStatus =
    | Success  // 成功
    | Failure  // 失敗
    | Partial  // 一部成功

module AutoJournalStatus =
    let toString = function
        | Success -> "SUCCESS"
        | Failure -> "FAILURE"
        | Partial -> "PARTIAL"

    let fromString = function
        | "SUCCESS" -> Some Success
        | "FAILURE" -> Some Failure
        | "PARTIAL" -> Some Partial
        | _ -> None

/// <summary>
/// 自動仕訳実行ログエンティティ
/// 処理状況の追跡と監査証跡
/// </summary>
type AutoJournalLog = {
    /// ログID
    Id: int64
    /// パターンID
    PatternId: int64
    /// 実行日時
    ExecutedAt: DateTime
    /// 処理件数（入力データ数）
    ProcessedCount: int
    /// 生成件数（仕訳生成数）
    GeneratedCount: int
    /// ステータス
    Status: AutoJournalStatus
    /// メッセージ
    Message: string option
    /// エラー詳細
    ErrorDetail: string option
    /// 作成日時
    CreatedAt: DateTime
}

module AutoJournalPattern =
    /// 新規パターンを作成
    let create patternCode patternName sourceTableName =
        {
            Id = 0L
            PatternCode = patternCode
            PatternName = patternName
            SourceTableName = sourceTableName
            Description = None
            IsActive = true
        }

    /// エンティティの同一性判定（PatternCode で判定）
    let equal (a: AutoJournalPattern) (b: AutoJournalPattern) =
        a.PatternCode = b.PatternCode

    /// ハッシュコード
    let hashCode (pattern: AutoJournalPattern) =
        pattern.PatternCode.GetHashCode()

module AutoJournalPatternItem =
    /// 借方明細を作成
    let createDebit patternId lineNumber accountCode amountExpression =
        {
            Id = 0L
            PatternId = patternId
            LineNumber = lineNumber
            DebitCreditType = Debit
            AccountCode = AccountCode.Create(accountCode)
            AmountExpression = amountExpression
            DescriptionTemplate = None
        }

    /// 貸方明細を作成
    let createCredit patternId lineNumber accountCode amountExpression =
        { createDebit patternId lineNumber accountCode amountExpression with
            DebitCreditType = Credit }

module AutoJournalLog =
    /// 成功ログを作成
    let createSuccess patternId processedCount generatedCount =
        {
            Id = 0L
            PatternId = patternId
            ExecutedAt = DateTime.UtcNow
            ProcessedCount = processedCount
            GeneratedCount = generatedCount
            Status = Success
            Message = Some "自動仕訳の生成が完了しました"
            ErrorDetail = None
            CreatedAt = DateTime.UtcNow
        }

    /// 失敗ログを作成
    let createFailure patternId processedCount message errorDetail =
        {
            Id = 0L
            PatternId = patternId
            ExecutedAt = DateTime.UtcNow
            ProcessedCount = processedCount
            GeneratedCount = 0
            Status = Failure
            Message = Some message
            ErrorDetail = Some errorDetail
            CreatedAt = DateTime.UtcNow
        }
