module AccountingSystem.Infrastructure.Repositories.JournalEntryRepository

open System
open AccountingSystem.Domain.Models
open AccountingSystem.Infrastructure.DAO
open Dapper
open Npgsql

/// 仕訳エントリを登録
let insertAsync (connectionString: string) (entry: JournalEntry) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            INSERT INTO "仕訳エントリ" (
                "伝票番号",
                "仕訳日",
                "摘要",
                "合計金額",
                "参照番号",
                "作成者",
                "作成日時",
                "更新日時"
            ) VALUES (
                @VoucherNumber,
                @EntryDate,
                @Description,
                @TotalAmount,
                @ReferenceNumber,
                @CreatedBy,
                CURRENT_TIMESTAMP,
                CURRENT_TIMESTAMP
            )
            RETURNING "伝票番号"
        """

        let parameters = JournalEntryDao.fromDomain entry
        let! result = conn.ExecuteScalarAsync<string>(sql, parameters)
        return result
    }

/// 伝票番号で検索
let findByVoucherNumberAsync (connectionString: string) (voucherNumber: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "伝票番号" AS "VoucherNumber",
                   "仕訳日" AS "EntryDate",
                   "摘要" AS "Description",
                   "合計金額" AS "TotalAmount",
                   "参照番号" AS "ReferenceNumber",
                   "作成者" AS "CreatedBy",
                   "作成日時" AS "CreatedAt",
                   "更新者" AS "UpdatedBy",
                   "更新日時" AS "UpdatedAt"
            FROM "仕訳エントリ"
            WHERE "伝票番号" = @VoucherNumber
        """

        let! results = conn.QueryAsync<JournalEntryDao>(sql, {| VoucherNumber = voucherNumber |})
        return results |> Seq.tryHead |> Option.map JournalEntryDao.toDomain
    }

/// 日付範囲で検索
let findByDateRangeAsync (connectionString: string) (fromDate: DateTime) (toDate: DateTime) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "伝票番号" AS "VoucherNumber",
                   "仕訳日" AS "EntryDate",
                   "摘要" AS "Description",
                   "合計金額" AS "TotalAmount",
                   "参照番号" AS "ReferenceNumber",
                   "作成者" AS "CreatedBy",
                   "作成日時" AS "CreatedAt",
                   "更新者" AS "UpdatedBy",
                   "更新日時" AS "UpdatedAt"
            FROM "仕訳エントリ"
            WHERE "仕訳日" BETWEEN @FromDate AND @ToDate
            ORDER BY "仕訳日", "伝票番号"
        """

        let! results = conn.QueryAsync<JournalEntryDao>(sql, {| FromDate = fromDate; ToDate = toDate |})
        return results |> Seq.map JournalEntryDao.toDomain |> Seq.toList
    }

/// 仕訳エントリを更新
let updateAsync (connectionString: string) (entry: JournalEntry) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            UPDATE "仕訳エントリ"
            SET "仕訳日" = @EntryDate,
                "摘要" = @Description,
                "合計金額" = @TotalAmount,
                "参照番号" = @ReferenceNumber,
                "更新者" = @UpdatedBy,
                "更新日時" = CURRENT_TIMESTAMP
            WHERE "伝票番号" = @VoucherNumber
        """

        let parameters = JournalEntryDao.fromDomain entry
        let! rowsAffected = conn.ExecuteAsync(sql, parameters)
        return rowsAffected
    }

/// 仕訳エントリを削除（明細も連鎖削除）
let deleteAsync (connectionString: string) (voucherNumber: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            DELETE FROM "仕訳エントリ"
            WHERE "伝票番号" = @VoucherNumber
        """

        let! rowsAffected = conn.ExecuteAsync(sql, {| VoucherNumber = voucherNumber |})
        return rowsAffected
    }
