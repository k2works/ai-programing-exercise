namespace AccountingSystem.Infrastructure.Persistence.DAO

open System
open AccountingSystem.Domain.Models.AutoJournalPattern
open AccountingSystem.Domain.Types

/// <summary>
/// 自動仕訳パターン DAO（Dapper マッピング用）
/// </summary>
[<CLIMutable>]
type AutoJournalPatternDao = {
    Id: int64
    PatternCode: string
    PatternName: string
    SourceTableName: string
    Description: string
    IsActive: bool
}

/// <summary>
/// 自動仕訳パターン明細 DAO（Dapper マッピング用）
/// </summary>
[<CLIMutable>]
type AutoJournalPatternItemDao = {
    Id: int64
    PatternId: int64
    LineNumber: int
    DebitCreditType: string
    AccountCode: string
    AmountExpression: string
    DescriptionTemplate: string
}

/// <summary>
/// 自動仕訳実行ログ DAO（Dapper マッピング用）
/// </summary>
[<CLIMutable>]
type AutoJournalLogDao = {
    Id: int64
    PatternId: int64
    ExecutedAt: DateTime
    ProcessedCount: int
    GeneratedCount: int
    Status: string
    Message: string
    ErrorDetail: string
    CreatedAt: DateTime
}

module AutoJournalPatternDao =
    /// DAO からドメインモデルへ変換
    let toDomain (dao: AutoJournalPatternDao) : AutoJournalPattern =
        {
            Id = dao.Id
            PatternCode = dao.PatternCode
            PatternName = dao.PatternName
            SourceTableName = dao.SourceTableName
            Description = if String.IsNullOrEmpty(dao.Description) then None else Some dao.Description
            IsActive = dao.IsActive
        }

    /// ドメインモデルから DAO へ変換（INSERT/UPDATE 用パラメータ）
    let fromDomain (model: AutoJournalPattern) =
        {|
            Id = model.Id
            PatternCode = model.PatternCode
            PatternName = model.PatternName
            SourceTableName = model.SourceTableName
            Description = model.Description |> Option.defaultValue null
            IsActive = model.IsActive
        |}

module AutoJournalPatternItemDao =
    /// DAO からドメインモデルへ変換
    let toDomain (dao: AutoJournalPatternItemDao) : AutoJournalPatternItem =
        {
            Id = dao.Id
            PatternId = dao.PatternId
            LineNumber = dao.LineNumber
            DebitCreditType = if dao.DebitCreditType = "D" then Debit else Credit
            AccountCode = AccountCode.Create(dao.AccountCode)
            AmountExpression = dao.AmountExpression
            DescriptionTemplate = if String.IsNullOrEmpty(dao.DescriptionTemplate) then None else Some dao.DescriptionTemplate
        }

    /// ドメインモデルから DAO へ変換（INSERT 用パラメータ）
    let fromDomain (patternId: int64) (model: AutoJournalPatternItem) =
        {|
            PatternId = patternId
            LineNumber = model.LineNumber
            DebitCreditType = if model.DebitCreditType = Debit then "D" else "C"
            AccountCode = model.AccountCode.Code
            AmountExpression = model.AmountExpression
            DescriptionTemplate = model.DescriptionTemplate |> Option.defaultValue null
        |}

module AutoJournalLogDao =
    /// DAO からドメインモデルへ変換
    let toDomain (dao: AutoJournalLogDao) : AutoJournalLog =
        {
            Id = dao.Id
            PatternId = dao.PatternId
            ExecutedAt = dao.ExecutedAt
            ProcessedCount = dao.ProcessedCount
            GeneratedCount = dao.GeneratedCount
            Status = AutoJournalStatus.fromString dao.Status |> Option.defaultValue Failure
            Message = if String.IsNullOrEmpty(dao.Message) then None else Some dao.Message
            ErrorDetail = if String.IsNullOrEmpty(dao.ErrorDetail) then None else Some dao.ErrorDetail
            CreatedAt = dao.CreatedAt
        }

    /// ドメインモデルから DAO へ変換（INSERT 用パラメータ）
    let fromDomain (model: AutoJournalLog) =
        {|
            PatternId = model.PatternId
            ExecutedAt = model.ExecutedAt
            ProcessedCount = model.ProcessedCount
            GeneratedCount = model.GeneratedCount
            Status = AutoJournalStatus.toString model.Status
            Message = model.Message |> Option.defaultValue null
            ErrorDetail = model.ErrorDetail |> Option.defaultValue null
        |}
