module AccountingSystem.Infrastructure.Repositories.JournalDetailRepository

open AccountingSystem.Domain.Models
open AccountingSystem.Infrastructure.DAO
open Dapper
open Npgsql

/// 仕訳明細を登録
let insertAsync (connectionString: string) (detail: JournalDetail) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            INSERT INTO "仕訳明細" (
                "伝票番号",
                "行番号",
                "勘定科目コード",
                "借方金額",
                "貸方金額",
                "摘要",
                "消費税額",
                "消費税率"
            ) VALUES (
                @VoucherNumber,
                @LineNumber,
                @AccountCode,
                @DebitAmount,
                @CreditAmount,
                @Description,
                @TaxAmount,
                @TaxRate
            )
        """

        let parameters = JournalDetailDao.fromDomain detail
        let! _ = conn.ExecuteAsync(sql, parameters)
        ()
    }

/// 仕訳明細を一括登録
let insertBatchAsync (connectionString: string) (details: JournalDetail list) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            INSERT INTO "仕訳明細" (
                "伝票番号",
                "行番号",
                "勘定科目コード",
                "借方金額",
                "貸方金額",
                "摘要",
                "消費税額",
                "消費税率"
            ) VALUES (
                @VoucherNumber,
                @LineNumber,
                @AccountCode,
                @DebitAmount,
                @CreditAmount,
                @Description,
                @TaxAmount,
                @TaxRate
            )
        """

        let parameters = details |> List.map JournalDetailDao.fromDomain |> List.toArray
        let! rowsAffected = conn.ExecuteAsync(sql, parameters)
        return rowsAffected
    }

/// 伝票番号で仕訳明細を検索
let findByVoucherNumberAsync (connectionString: string) (voucherNumber: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "伝票番号" AS "VoucherNumber",
                   "行番号" AS "LineNumber",
                   "勘定科目コード" AS "AccountCode",
                   "借方金額" AS "DebitAmount",
                   "貸方金額" AS "CreditAmount",
                   "摘要" AS "Description",
                   "消費税額" AS "TaxAmount",
                   "消費税率" AS "TaxRate"
            FROM "仕訳明細"
            WHERE "伝票番号" = @VoucherNumber
            ORDER BY "行番号"
        """

        let! results = conn.QueryAsync<JournalDetailDao>(sql, {| VoucherNumber = voucherNumber |})
        return results |> Seq.map JournalDetailDao.toDomain |> Seq.toList
    }

/// 勘定科目コードで仕訳明細を検索
let findByAccountCodeAsync (connectionString: string) (accountCode: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "伝票番号" AS "VoucherNumber",
                   "行番号" AS "LineNumber",
                   "勘定科目コード" AS "AccountCode",
                   "借方金額" AS "DebitAmount",
                   "貸方金額" AS "CreditAmount",
                   "摘要" AS "Description",
                   "消費税額" AS "TaxAmount",
                   "消費税率" AS "TaxRate"
            FROM "仕訳明細"
            WHERE "勘定科目コード" = @AccountCode
            ORDER BY "伝票番号", "行番号"
        """

        let! results = conn.QueryAsync<JournalDetailDao>(sql, {| AccountCode = accountCode |})
        return results |> Seq.map JournalDetailDao.toDomain |> Seq.toList
    }

/// 伝票番号の借方・貸方合計を取得
let getBalanceAsync (connectionString: string) (voucherNumber: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT
                SUM("借方金額") as debit_total,
                SUM("貸方金額") as credit_total
            FROM "仕訳明細"
            WHERE "伝票番号" = @VoucherNumber
        """

        let! result = conn.QuerySingleOrDefaultAsync<{| debit_total: decimal; credit_total: decimal |}>(sql, {| VoucherNumber = voucherNumber |})
        return (result.debit_total, result.credit_total)
    }

/// 仕訳明細を削除
let deleteByVoucherNumberAsync (connectionString: string) (voucherNumber: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            DELETE FROM "仕訳明細"
            WHERE "伝票番号" = @VoucherNumber
        """

        let! rowsAffected = conn.ExecuteAsync(sql, {| VoucherNumber = voucherNumber |})
        return rowsAffected
    }
