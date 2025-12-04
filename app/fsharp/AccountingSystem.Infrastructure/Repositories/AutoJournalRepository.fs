namespace AccountingSystem.Infrastructure.Repositories

open System
open System.Threading.Tasks
open Dapper
open Npgsql
open AccountingSystem.Domain.Models.AutoJournalPattern
open AccountingSystem.Domain.Types
open AccountingSystem.Application.Repositories

/// パターンDAO
type PatternDao = {
    Id: int64
    PatternCode: string
    PatternName: string
    SourceTableName: string
    Description: string
    IsActive: bool
}

/// パターン明細DAO
type PatternItemDao = {
    Id: int64
    PatternId: int64
    LineNumber: int
    DebitCreditType: string
    AccountCode: string
    AmountExpression: string
    DescriptionTemplate: string
}

/// ログDAO
type LogDao = {
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

/// <summary>
/// 自動仕訳リポジトリの PostgreSQL 実装
/// </summary>
type AutoJournalRepository(connection: NpgsqlConnection) =

    let mapToPattern (dao: PatternDao) : AutoJournalPattern =
        {
            Id = dao.Id
            PatternCode = dao.PatternCode
            PatternName = dao.PatternName
            SourceTableName = dao.SourceTableName
            Description = if String.IsNullOrEmpty(dao.Description) then None else Some dao.Description
            IsActive = dao.IsActive
        }

    let mapToPatternItem (dao: PatternItemDao) : AutoJournalPatternItem =
        {
            Id = dao.Id
            PatternId = dao.PatternId
            LineNumber = dao.LineNumber
            DebitCreditType = if dao.DebitCreditType = "D" then Debit else Credit
            AccountCode = AccountCode.Create(dao.AccountCode)
            AmountExpression = dao.AmountExpression
            DescriptionTemplate = if String.IsNullOrEmpty(dao.DescriptionTemplate) then None else Some dao.DescriptionTemplate
        }

    let mapToLog (dao: LogDao) : AutoJournalLog =
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

    interface IAutoJournalRepository with

        member this.GetPatternsAsync(query: AutoJournalPatternQuery) : Task<AutoJournalPattern list> =
            task {
                let baseSql = """
                    SELECT
                        id as "Id",
                        "パターンコード" as "PatternCode",
                        "パターン名" as "PatternName",
                        "ソーステーブル名" as "SourceTableName",
                        "説明" as "Description",
                        "有効フラグ" as "IsActive"
                    FROM "自動仕訳パターン"
                    WHERE 1=1
                """

                let conditions = ResizeArray<string>()
                let parameters = DynamicParameters()

                match query.PatternCode with
                | Some code ->
                    conditions.Add("AND \"パターンコード\" = @PatternCode")
                    parameters.Add("PatternCode", code)
                | None -> ()

                match query.IsActive with
                | Some active ->
                    conditions.Add("AND \"有効フラグ\" = @IsActive")
                    parameters.Add("IsActive", active)
                | None -> ()

                let sql = baseSql + String.Join(" ", conditions) + " ORDER BY \"パターンコード\""

                let! results = connection.QueryAsync<PatternDao>(sql, parameters)

                return results |> Seq.map mapToPattern |> Seq.toList
            }

        member this.GetPatternByCodeAsync(patternCode: string) : Task<AutoJournalPattern option> =
            task {
                let sql = """
                    SELECT
                        id as "Id",
                        "パターンコード" as "PatternCode",
                        "パターン名" as "PatternName",
                        "ソーステーブル名" as "SourceTableName",
                        "説明" as "Description",
                        "有効フラグ" as "IsActive"
                    FROM "自動仕訳パターン"
                    WHERE "パターンコード" = @PatternCode
                """

                let! result = connection.QueryFirstOrDefaultAsync<PatternDao>(sql, {| PatternCode = patternCode |})

                if isNull (box result) then
                    return None
                else
                    return Some (mapToPattern result)
            }

        member this.GetPatternItemsAsync(patternId: int64) : Task<AutoJournalPatternItem list> =
            task {
                let sql = """
                    SELECT
                        id as "Id",
                        "パターンID" as "PatternId",
                        "行番号" as "LineNumber",
                        "貸借区分" as "DebitCreditType",
                        "勘定科目コード" as "AccountCode",
                        "金額式" as "AmountExpression",
                        "摘要テンプレート" as "DescriptionTemplate"
                    FROM "自動仕訳パターン明細"
                    WHERE "パターンID" = @PatternId
                    ORDER BY "行番号"
                """

                let! results = connection.QueryAsync<PatternItemDao>(sql, {| PatternId = patternId |})

                return results |> Seq.map mapToPatternItem |> Seq.toList
            }

        member this.SavePatternAsync(pattern: AutoJournalPattern) : Task<AutoJournalPattern> =
            task {
                if pattern.Id = 0L then
                    let sql = """
                        INSERT INTO "自動仕訳パターン" (
                            "パターンコード", "パターン名", "ソーステーブル名", "説明", "有効フラグ"
                        ) VALUES (
                            @PatternCode, @PatternName, @SourceTableName, @Description, @IsActive
                        ) RETURNING id
                    """
                    let! id = connection.ExecuteScalarAsync<int64>(sql, {|
                        PatternCode = pattern.PatternCode
                        PatternName = pattern.PatternName
                        SourceTableName = pattern.SourceTableName
                        Description = pattern.Description |> Option.defaultValue null
                        IsActive = pattern.IsActive
                    |})
                    return { pattern with Id = id }
                else
                    let sql = """
                        UPDATE "自動仕訳パターン"
                        SET
                            "パターン名" = @PatternName,
                            "ソーステーブル名" = @SourceTableName,
                            "説明" = @Description,
                            "有効フラグ" = @IsActive,
                            "更新日時" = CURRENT_TIMESTAMP
                        WHERE id = @Id
                    """
                    let! _ = connection.ExecuteAsync(sql, {|
                        Id = pattern.Id
                        PatternName = pattern.PatternName
                        SourceTableName = pattern.SourceTableName
                        Description = pattern.Description |> Option.defaultValue null
                        IsActive = pattern.IsActive
                    |})
                    return pattern
            }

        member this.SavePatternItemsAsync (patternId: int64) (items: AutoJournalPatternItem list) : Task<unit> =
            task {
                let deleteSql = """DELETE FROM "自動仕訳パターン明細" WHERE "パターンID" = @PatternId"""
                let! _ = connection.ExecuteAsync(deleteSql, {| PatternId = patternId |})

                let insertSql = """
                    INSERT INTO "自動仕訳パターン明細" (
                        "パターンID", "行番号", "貸借区分", "勘定科目コード", "金額式", "摘要テンプレート"
                    ) VALUES (
                        @PatternId, @LineNumber, @DebitCreditType, @AccountCode, @AmountExpression, @DescriptionTemplate
                    )
                """

                for item in items do
                    let dcType = if item.DebitCreditType = Debit then "D" else "C"
                    let! _ = connection.ExecuteAsync(insertSql, {|
                        PatternId = patternId
                        LineNumber = item.LineNumber
                        DebitCreditType = dcType
                        AccountCode = item.AccountCode.Code
                        AmountExpression = item.AmountExpression
                        DescriptionTemplate = item.DescriptionTemplate |> Option.defaultValue null
                    |})
                    ()
            }

        member this.SaveLogAsync(log: AutoJournalLog) : Task<AutoJournalLog> =
            task {
                let sql = """
                    INSERT INTO "自動仕訳実行ログ" (
                        "パターンID", "実行日時", "処理件数", "生成件数", "ステータス", "メッセージ", "エラー詳細"
                    ) VALUES (
                        @PatternId, @ExecutedAt, @ProcessedCount, @GeneratedCount, @Status, @Message, @ErrorDetail
                    ) RETURNING id, "作成日時"
                """
                let! result = connection.QueryFirstAsync<{| id: int64; 作成日時: DateTime |}>(sql, {|
                    PatternId = log.PatternId
                    ExecutedAt = log.ExecutedAt
                    ProcessedCount = log.ProcessedCount
                    GeneratedCount = log.GeneratedCount
                    Status = AutoJournalStatus.toString log.Status
                    Message = log.Message |> Option.defaultValue null
                    ErrorDetail = log.ErrorDetail |> Option.defaultValue null
                |})
                return { log with Id = result.id; CreatedAt = result.作成日時 }
            }

        member this.GetLogsByPatternIdAsync (patternId: int64) (limit: int) : Task<AutoJournalLog list> =
            task {
                let sql = """
                    SELECT
                        id as "Id",
                        "パターンID" as "PatternId",
                        "実行日時" as "ExecutedAt",
                        "処理件数" as "ProcessedCount",
                        "生成件数" as "GeneratedCount",
                        "ステータス" as "Status",
                        "メッセージ" as "Message",
                        "エラー詳細" as "ErrorDetail",
                        "作成日時" as "CreatedAt"
                    FROM "自動仕訳実行ログ"
                    WHERE "パターンID" = @PatternId
                    ORDER BY "実行日時" DESC
                    LIMIT @Limit
                """

                let! results = connection.QueryAsync<LogDao>(sql, {| PatternId = patternId; Limit = limit |})

                return results |> Seq.map mapToLog |> Seq.toList
            }
