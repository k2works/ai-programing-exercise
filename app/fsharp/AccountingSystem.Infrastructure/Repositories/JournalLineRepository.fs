module AccountingSystem.Infrastructure.Repositories.JournalLineRepository

open AccountingSystem.Domain.Models.JournalLine
open Dapper
open Npgsql

module Dao = AccountingSystem.Infrastructure.DAO.JournalLineDao

/// 仕訳明細を登録
let insertAsync (connectionString: string) (line: JournalLine) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            INSERT INTO "仕訳明細V2" (
                "仕訳伝票番号",
                "仕訳行番号",
                "行摘要",
                "作成日時",
                "更新日時"
            ) VALUES (
                @VoucherNumber,
                @LineNumber,
                @Description,
                CURRENT_TIMESTAMP,
                CURRENT_TIMESTAMP
            )
        """

        let parameters = Dao.JournalLineDao.fromDomain line
        let! _ = conn.ExecuteAsync(sql, parameters)
        ()
    }

/// 仕訳明細を一括登録
let insertBatchAsync (connectionString: string) (lines: JournalLine list) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            INSERT INTO "仕訳明細V2" (
                "仕訳伝票番号",
                "仕訳行番号",
                "行摘要",
                "作成日時",
                "更新日時"
            ) VALUES (
                @VoucherNumber,
                @LineNumber,
                @Description,
                CURRENT_TIMESTAMP,
                CURRENT_TIMESTAMP
            )
        """

        let parameters = lines |> List.map Dao.JournalLineDao.fromDomain |> List.toArray
        let! rowsAffected = conn.ExecuteAsync(sql, parameters)
        return rowsAffected
    }

/// 伝票番号で仕訳明細を検索
let findByVoucherNumberAsync (connectionString: string) (voucherNumber: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "仕訳伝票番号" AS "VoucherNumber",
                   "仕訳行番号" AS "LineNumber",
                   "行摘要" AS "Description",
                   "作成日時" AS "CreatedAt",
                   "更新日時" AS "UpdatedAt"
            FROM "仕訳明細V2"
            WHERE "仕訳伝票番号" = @VoucherNumber
            ORDER BY "仕訳行番号"
        """

        let! results = conn.QueryAsync<Dao.JournalLineDao>(sql, {| VoucherNumber = voucherNumber |})
        return results |> Seq.map Dao.JournalLineDao.toDomain |> Seq.toList
    }

/// 仕訳明細を削除
let deleteByVoucherNumberAsync (connectionString: string) (voucherNumber: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            DELETE FROM "仕訳明細V2"
            WHERE "仕訳伝票番号" = @VoucherNumber
        """

        let! rowsAffected = conn.ExecuteAsync(sql, {| VoucherNumber = voucherNumber |})
        return rowsAffected
    }
