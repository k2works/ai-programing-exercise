module AccountingSystem.Infrastructure.Repositories.JournalLineItemRepository

open AccountingSystem.Domain.Models.JournalLineItem
open Dapper
open Npgsql

module Dao = AccountingSystem.Infrastructure.DAO.JournalLineItemDao

/// 仕訳貸借明細を登録
let insertAsync (connectionString: string) (item: JournalLineItem) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            INSERT INTO "仕訳貸借明細" (
                "仕訳伝票番号",
                "仕訳行番号",
                "仕訳行貸借区分",
                "通貨コード",
                "為替レート",
                "部門コード",
                "プロジェクトコード",
                "勘定科目コード",
                "補助科目コード",
                "仕訳金額",
                "基軸換算仕訳金額",
                "消費税区分",
                "消費税率",
                "消費税計算区分",
                "期日",
                "資金繰フラグ",
                "セグメントコード",
                "相手勘定科目コード",
                "相手補助科目コード",
                "付箋コード",
                "付箋内容",
                "作成日時",
                "更新日時"
            ) VALUES (
                @VoucherNumber,
                @LineNumber,
                @DebitCreditType,
                @CurrencyCode,
                @ExchangeRate,
                @DepartmentCode,
                @ProjectCode,
                @AccountCode,
                @SubAccountCode,
                @Amount,
                @BaseAmount,
                @TaxCategory,
                @TaxRate,
                @TaxCalculationType,
                @DueDate,
                @CashFlowFlag,
                @SegmentCode,
                @CounterAccountCode,
                @CounterSubAccountCode,
                @MemoCode,
                @MemoContent,
                CURRENT_TIMESTAMP,
                CURRENT_TIMESTAMP
            )
        """

        let parameters = Dao.JournalLineItemDao.fromDomain item
        let! _ = conn.ExecuteAsync(sql, parameters)
        ()
    }

/// 仕訳貸借明細を一括登録
let insertBatchAsync (connectionString: string) (items: JournalLineItem list) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            INSERT INTO "仕訳貸借明細" (
                "仕訳伝票番号",
                "仕訳行番号",
                "仕訳行貸借区分",
                "通貨コード",
                "為替レート",
                "部門コード",
                "プロジェクトコード",
                "勘定科目コード",
                "補助科目コード",
                "仕訳金額",
                "基軸換算仕訳金額",
                "消費税区分",
                "消費税率",
                "消費税計算区分",
                "期日",
                "資金繰フラグ",
                "セグメントコード",
                "相手勘定科目コード",
                "相手補助科目コード",
                "付箋コード",
                "付箋内容",
                "作成日時",
                "更新日時"
            ) VALUES (
                @VoucherNumber,
                @LineNumber,
                @DebitCreditType,
                @CurrencyCode,
                @ExchangeRate,
                @DepartmentCode,
                @ProjectCode,
                @AccountCode,
                @SubAccountCode,
                @Amount,
                @BaseAmount,
                @TaxCategory,
                @TaxRate,
                @TaxCalculationType,
                @DueDate,
                @CashFlowFlag,
                @SegmentCode,
                @CounterAccountCode,
                @CounterSubAccountCode,
                @MemoCode,
                @MemoContent,
                CURRENT_TIMESTAMP,
                CURRENT_TIMESTAMP
            )
        """

        let parameters = items |> List.map Dao.JournalLineItemDao.fromDomain |> List.toArray
        let! rowsAffected = conn.ExecuteAsync(sql, parameters)
        return rowsAffected
    }

/// 伝票番号で仕訳貸借明細を検索
let findByVoucherNumberAsync (connectionString: string) (voucherNumber: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "仕訳伝票番号" AS "VoucherNumber",
                   "仕訳行番号" AS "LineNumber",
                   "仕訳行貸借区分" AS "DebitCreditType",
                   "通貨コード" AS "CurrencyCode",
                   "為替レート" AS "ExchangeRate",
                   "部門コード" AS "DepartmentCode",
                   "プロジェクトコード" AS "ProjectCode",
                   "勘定科目コード" AS "AccountCode",
                   "補助科目コード" AS "SubAccountCode",
                   "仕訳金額" AS "Amount",
                   "基軸換算仕訳金額" AS "BaseAmount",
                   "消費税区分" AS "TaxCategory",
                   "消費税率" AS "TaxRate",
                   "消費税計算区分" AS "TaxCalculationType",
                   "期日" AS "DueDate",
                   "資金繰フラグ" AS "CashFlowFlag",
                   "セグメントコード" AS "SegmentCode",
                   "相手勘定科目コード" AS "CounterAccountCode",
                   "相手補助科目コード" AS "CounterSubAccountCode",
                   "付箋コード" AS "MemoCode",
                   "付箋内容" AS "MemoContent",
                   "作成日時" AS "CreatedAt",
                   "更新日時" AS "UpdatedAt"
            FROM "仕訳貸借明細"
            WHERE "仕訳伝票番号" = @VoucherNumber
            ORDER BY "仕訳行番号", "仕訳行貸借区分"
        """

        let! results = conn.QueryAsync<Dao.JournalLineItemDao>(sql, {| VoucherNumber = voucherNumber |})
        return results |> Seq.map Dao.JournalLineItemDao.toDomain |> Seq.toList
    }

/// 勘定科目コードで仕訳貸借明細を検索
let findByAccountCodeAsync (connectionString: string) (accountCode: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "仕訳伝票番号" AS "VoucherNumber",
                   "仕訳行番号" AS "LineNumber",
                   "仕訳行貸借区分" AS "DebitCreditType",
                   "通貨コード" AS "CurrencyCode",
                   "為替レート" AS "ExchangeRate",
                   "部門コード" AS "DepartmentCode",
                   "プロジェクトコード" AS "ProjectCode",
                   "勘定科目コード" AS "AccountCode",
                   "補助科目コード" AS "SubAccountCode",
                   "仕訳金額" AS "Amount",
                   "基軸換算仕訳金額" AS "BaseAmount",
                   "消費税区分" AS "TaxCategory",
                   "消費税率" AS "TaxRate",
                   "消費税計算区分" AS "TaxCalculationType",
                   "期日" AS "DueDate",
                   "資金繰フラグ" AS "CashFlowFlag",
                   "セグメントコード" AS "SegmentCode",
                   "相手勘定科目コード" AS "CounterAccountCode",
                   "相手補助科目コード" AS "CounterSubAccountCode",
                   "付箋コード" AS "MemoCode",
                   "付箋内容" AS "MemoContent",
                   "作成日時" AS "CreatedAt",
                   "更新日時" AS "UpdatedAt"
            FROM "仕訳貸借明細"
            WHERE "勘定科目コード" = @AccountCode
            ORDER BY "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分"
        """

        let! results = conn.QueryAsync<Dao.JournalLineItemDao>(sql, {| AccountCode = accountCode |})
        return results |> Seq.map Dao.JournalLineItemDao.toDomain |> Seq.toList
    }

/// 伝票番号の借方・貸方合計を取得
let getBalanceAsync (connectionString: string) (voucherNumber: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT
                COALESCE(SUM(CASE WHEN "仕訳行貸借区分" = 'D' THEN "仕訳金額" ELSE 0 END), 0) as debit_total,
                COALESCE(SUM(CASE WHEN "仕訳行貸借区分" = 'C' THEN "仕訳金額" ELSE 0 END), 0) as credit_total
            FROM "仕訳貸借明細"
            WHERE "仕訳伝票番号" = @VoucherNumber
        """

        let! result = conn.QuerySingleOrDefaultAsync<{| debit_total: decimal; credit_total: decimal |}>(sql, {| VoucherNumber = voucherNumber |})
        return (result.debit_total, result.credit_total)
    }

/// 仕訳貸借明細を削除
let deleteByVoucherNumberAsync (connectionString: string) (voucherNumber: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            DELETE FROM "仕訳貸借明細"
            WHERE "仕訳伝票番号" = @VoucherNumber
        """

        let! rowsAffected = conn.ExecuteAsync(sql, {| VoucherNumber = voucherNumber |})
        return rowsAffected
    }
