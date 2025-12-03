module AccountingSystem.Infrastructure.Repositories.JournalRepository

open System
open AccountingSystem.Domain.Models.Journal
open Dapper
open Npgsql

module Dao = AccountingSystem.Infrastructure.DAO.JournalDao

/// 仕訳を登録
let insertAsync (connectionString: string) (journal: Journal) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            INSERT INTO "仕訳" (
                "仕訳伝票番号",
                "起票日",
                "入力日",
                "決算仕訳フラグ",
                "単振フラグ",
                "仕訳伝票区分",
                "定期計上フラグ",
                "社員コード",
                "部門コード",
                "赤伝フラグ",
                "赤黒伝票番号",
                "作成日時",
                "更新日時"
            ) VALUES (
                @VoucherNumber,
                @PostingDate,
                @EntryDate,
                @SettlementFlag,
                @SingleEntryFlag,
                @VoucherType,
                @RecurringFlag,
                @EmployeeCode,
                @DepartmentCode,
                @RedSlipFlag,
                @RedBlackVoucherNumber,
                CURRENT_TIMESTAMP,
                CURRENT_TIMESTAMP
            )
            RETURNING "仕訳伝票番号"
        """

        let parameters = Dao.JournalDao.fromDomain journal
        let! result = conn.ExecuteScalarAsync<string>(sql, parameters)
        return result
    }

/// 伝票番号で検索
let findByVoucherNumberAsync (connectionString: string) (voucherNumber: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "仕訳伝票番号" AS "VoucherNumber",
                   "起票日" AS "PostingDate",
                   "入力日" AS "EntryDate",
                   "決算仕訳フラグ" AS "SettlementFlag",
                   "単振フラグ" AS "SingleEntryFlag",
                   "仕訳伝票区分" AS "VoucherType",
                   "定期計上フラグ" AS "RecurringFlag",
                   "社員コード" AS "EmployeeCode",
                   "部門コード" AS "DepartmentCode",
                   "赤伝フラグ" AS "RedSlipFlag",
                   "赤黒伝票番号" AS "RedBlackVoucherNumber",
                   "作成日時" AS "CreatedAt",
                   "更新日時" AS "UpdatedAt"
            FROM "仕訳"
            WHERE "仕訳伝票番号" = @VoucherNumber
        """

        let! results = conn.QueryAsync<Dao.JournalDao>(sql, {| VoucherNumber = voucherNumber |})
        return results |> Seq.tryHead |> Option.map Dao.JournalDao.toDomain
    }

/// 起票日範囲で検索
let findByDateRangeAsync (connectionString: string) (fromDate: DateTime) (toDate: DateTime) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "仕訳伝票番号" AS "VoucherNumber",
                   "起票日" AS "PostingDate",
                   "入力日" AS "EntryDate",
                   "決算仕訳フラグ" AS "SettlementFlag",
                   "単振フラグ" AS "SingleEntryFlag",
                   "仕訳伝票区分" AS "VoucherType",
                   "定期計上フラグ" AS "RecurringFlag",
                   "社員コード" AS "EmployeeCode",
                   "部門コード" AS "DepartmentCode",
                   "赤伝フラグ" AS "RedSlipFlag",
                   "赤黒伝票番号" AS "RedBlackVoucherNumber",
                   "作成日時" AS "CreatedAt",
                   "更新日時" AS "UpdatedAt"
            FROM "仕訳"
            WHERE "起票日" BETWEEN @FromDate AND @ToDate
            ORDER BY "起票日", "仕訳伝票番号"
        """

        let! results = conn.QueryAsync<Dao.JournalDao>(sql, {| FromDate = fromDate; ToDate = toDate |})
        return results |> Seq.map Dao.JournalDao.toDomain |> Seq.toList
    }

/// 仕訳を更新
let updateAsync (connectionString: string) (journal: Journal) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            UPDATE "仕訳"
            SET "起票日" = @PostingDate,
                "入力日" = @EntryDate,
                "決算仕訳フラグ" = @SettlementFlag,
                "単振フラグ" = @SingleEntryFlag,
                "仕訳伝票区分" = @VoucherType,
                "定期計上フラグ" = @RecurringFlag,
                "社員コード" = @EmployeeCode,
                "部門コード" = @DepartmentCode,
                "赤伝フラグ" = @RedSlipFlag,
                "赤黒伝票番号" = @RedBlackVoucherNumber,
                "更新日時" = CURRENT_TIMESTAMP
            WHERE "仕訳伝票番号" = @VoucherNumber
        """

        let parameters = Dao.JournalDao.fromDomain journal
        let! rowsAffected = conn.ExecuteAsync(sql, parameters)
        return rowsAffected
    }

/// 仕訳を削除（明細も連鎖削除）
let deleteAsync (connectionString: string) (voucherNumber: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            DELETE FROM "仕訳"
            WHERE "仕訳伝票番号" = @VoucherNumber
        """

        let! rowsAffected = conn.ExecuteAsync(sql, {| VoucherNumber = voucherNumber |})
        return rowsAffected
    }
