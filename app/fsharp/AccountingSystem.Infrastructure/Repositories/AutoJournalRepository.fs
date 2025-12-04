module AccountingSystem.Infrastructure.Repositories.AutoJournalRepository

open System
open Dapper
open Npgsql
open AccountingSystem.Domain.Models.AutoJournalPattern
open AccountingSystem.Application.Repositories
open AccountingSystem.Infrastructure.DAO

/// クエリ条件に基づいてパターン一覧を取得
let getPatternsAsync (connectionString: string) (query: AutoJournalPatternQuery) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

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

        let! results = conn.QueryAsync<AutoJournalPatternDao>(sql, parameters)

        return results |> Seq.map AutoJournalPatternDao.toDomain |> Seq.toList
    }

/// パターンコードでパターンを取得
let getPatternByCodeAsync (connectionString: string) (patternCode: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

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

        let! results = conn.QueryAsync<AutoJournalPatternDao>(sql, {| PatternCode = patternCode |})

        return results |> Seq.tryHead |> Option.map AutoJournalPatternDao.toDomain
    }

/// パターン明細を取得
let getPatternItemsAsync (connectionString: string) (patternId: int64) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

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

        let! results = conn.QueryAsync<AutoJournalPatternItemDao>(sql, {| PatternId = patternId |})

        return results |> Seq.map AutoJournalPatternItemDao.toDomain |> Seq.toList
    }

/// パターンを保存（新規作成または更新）
let savePatternAsync (connectionString: string) (pattern: AutoJournalPattern) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        if pattern.Id = 0L then
            let sql = """
                INSERT INTO "自動仕訳パターン" (
                    "パターンコード", "パターン名", "ソーステーブル名", "説明", "有効フラグ"
                ) VALUES (
                    @PatternCode, @PatternName, @SourceTableName, @Description, @IsActive
                ) RETURNING id
            """
            let parameters = AutoJournalPatternDao.fromDomain pattern
            let! id = conn.ExecuteScalarAsync<int64>(sql, parameters)
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
            let parameters = AutoJournalPatternDao.fromDomain pattern
            let! _ = conn.ExecuteAsync(sql, parameters)
            return pattern
    }

/// パターン明細を保存（既存明細を削除して置き換え）
let savePatternItemsAsync (connectionString: string) (patternId: int64) (items: AutoJournalPatternItem list) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let deleteSql = """DELETE FROM "自動仕訳パターン明細" WHERE "パターンID" = @PatternId"""
        let! _ = conn.ExecuteAsync(deleteSql, {| PatternId = patternId |})

        let insertSql = """
            INSERT INTO "自動仕訳パターン明細" (
                "パターンID", "行番号", "貸借区分", "勘定科目コード", "金額式", "摘要テンプレート"
            ) VALUES (
                @PatternId, @LineNumber, @DebitCreditType, @AccountCode, @AmountExpression, @DescriptionTemplate
            )
        """

        for item in items do
            let parameters = AutoJournalPatternItemDao.fromDomain patternId item
            let! _ = conn.ExecuteAsync(insertSql, parameters)
            ()
    }

/// 実行ログを保存
let saveLogAsync (connectionString: string) (log: AutoJournalLog) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            INSERT INTO "自動仕訳実行ログ" (
                "パターンID", "実行日時", "処理件数", "生成件数", "ステータス", "メッセージ", "エラー詳細"
            ) VALUES (
                @PatternId, @ExecutedAt, @ProcessedCount, @GeneratedCount, @Status, @Message, @ErrorDetail
            ) RETURNING id, "作成日時"
        """
        let parameters = AutoJournalLogDao.fromDomain log
        let! result = conn.QueryFirstAsync<{| id: int64; 作成日時: DateTime |}>(sql, parameters)
        return { log with Id = result.id; CreatedAt = result.作成日時 }
    }

/// パターンIDで実行ログを取得
let getLogsByPatternIdAsync (connectionString: string) (patternId: int64) (limit: int) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

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

        let! results = conn.QueryAsync<AutoJournalLogDao>(sql, {| PatternId = patternId; Limit = limit |})

        return results |> Seq.map AutoJournalLogDao.toDomain |> Seq.toList
    }

/// <summary>
/// IAutoJournalRepository インターフェースのアダプター実装
/// module 関数をインターフェース経由で利用可能にする
/// </summary>
type AutoJournalRepositoryAdapter(connectionString: string) =
    interface IAutoJournalRepository with
        member _.GetPatternsAsync(query) =
            getPatternsAsync connectionString query

        member _.GetPatternByCodeAsync(patternCode) =
            getPatternByCodeAsync connectionString patternCode

        member _.GetPatternItemsAsync(patternId) =
            getPatternItemsAsync connectionString patternId

        member _.SavePatternAsync(pattern) =
            savePatternAsync connectionString pattern

        member _.SavePatternItemsAsync patternId items =
            savePatternItemsAsync connectionString patternId items

        member _.SaveLogAsync(log) =
            saveLogAsync connectionString log

        member _.GetLogsByPatternIdAsync patternId limit =
            getLogsByPatternIdAsync connectionString patternId limit
