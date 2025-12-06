module AccountingSystem.Infrastructure.Persistence.Repositories.JournalRepository

open System
open AccountingSystem.Domain.Models.Journal
open AccountingSystem.Domain.Models.JournalLine
open AccountingSystem.Domain.Models.JournalLineItem
open Dapper
open Npgsql

module JournalDao = AccountingSystem.Infrastructure.DAO.JournalDao
module JournalLineDao = AccountingSystem.Infrastructure.DAO.JournalLineDao
module JournalLineItemDao = AccountingSystem.Infrastructure.DAO.JournalLineItemDao

/// 仕訳集約を登録（Journal + Lines + Items）
let insertAsync (connectionString: string) (journal: Journal) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()
        use! transaction = conn.BeginTransactionAsync()

        try
            // 1. 仕訳ヘッダーを登録
            let journalSql = """
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
            """
            let journalParams = JournalDao.JournalDao.fromDomain journal
            let! _ = conn.ExecuteAsync(journalSql, journalParams, transaction)

            // 2. 仕訳明細を登録
            let lineSql = """
                INSERT INTO "仕訳明細" (
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
            for line in journal.Lines do
                let lineParams = JournalLineDao.JournalLineDao.fromDomain line
                let! _ = conn.ExecuteAsync(lineSql, lineParams, transaction)

                // 3. 仕訳貸借明細を登録
                let itemSql = """
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
                for item in line.Items do
                    let itemParams = JournalLineItemDao.JournalLineItemDao.fromDomain item
                    let! _ = conn.ExecuteAsync(itemSql, itemParams, transaction)
                    ()

            do! transaction.CommitAsync()
            return journal.VoucherNumber.Number
        with ex ->
            do! transaction.RollbackAsync()
            return raise ex
    }

/// 伝票番号で検索（集約全体を取得）
let findByVoucherNumberAsync (connectionString: string) (voucherNumber: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        // 1. 仕訳ヘッダーを取得
        let journalSql = """
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
        let! journalResults = conn.QueryAsync<JournalDao.JournalDao>(journalSql, {| VoucherNumber = voucherNumber |})
        let journalDao = journalResults |> Seq.tryHead

        match journalDao with
        | None -> return None
        | Some jDao ->
            // 2. 仕訳明細を取得
            let lineSql = """
                SELECT "仕訳伝票番号" AS "VoucherNumber",
                       "仕訳行番号" AS "LineNumber",
                       "行摘要" AS "Description",
                       "作成日時" AS "CreatedAt",
                       "更新日時" AS "UpdatedAt"
                FROM "仕訳明細"
                WHERE "仕訳伝票番号" = @VoucherNumber
                ORDER BY "仕訳行番号"
            """
            let! lineResults = conn.QueryAsync<JournalLineDao.JournalLineDao>(lineSql, {| VoucherNumber = voucherNumber |})

            // 3. 仕訳貸借明細を取得
            let itemSql = """
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
            let! itemResults = conn.QueryAsync<JournalLineItemDao.JournalLineItemDao>(itemSql, {| VoucherNumber = voucherNumber |})

            // 集約を組み立て
            let itemsByLine =
                itemResults
                |> Seq.map JournalLineItemDao.JournalLineItemDao.toDomain
                |> Seq.groupBy (fun item -> item.LineNumber)
                |> Map.ofSeq

            let lines =
                lineResults
                |> Seq.map (fun lineDao ->
                    let baseLine = JournalLineDao.JournalLineDao.toDomain lineDao
                    let items = itemsByLine |> Map.tryFind baseLine.LineNumber |> Option.defaultValue Seq.empty |> Seq.toList
                    { baseLine with Items = items })
                |> Seq.toList

            let journal = JournalDao.JournalDao.toDomain jDao
            return Some { journal with Lines = lines }
    }

/// 起票日範囲で検索（明細も含めて取得）
let findByDateRangeAsync (connectionString: string) (fromDate: DateTime) (toDate: DateTime) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        // 1. 仕訳ヘッダーを取得
        let journalSql = """
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
        let! journalResults = conn.QueryAsync<JournalDao.JournalDao>(journalSql, {| FromDate = fromDate; ToDate = toDate |})
        let journals = journalResults |> Seq.toList

        if journals.IsEmpty then
            return []
        else
            // 対象の伝票番号リストを取得
            let voucherNumbers = journals |> List.map (fun j -> j.VoucherNumber)

            // 2. 仕訳明細を一括取得
            let lineSql = """
                SELECT "仕訳伝票番号" AS "VoucherNumber",
                       "仕訳行番号" AS "LineNumber",
                       "行摘要" AS "Description",
                       "作成日時" AS "CreatedAt",
                       "更新日時" AS "UpdatedAt"
                FROM "仕訳明細"
                WHERE "仕訳伝票番号" = ANY(@VoucherNumbers)
                ORDER BY "仕訳伝票番号", "仕訳行番号"
            """
            let! lineResults = conn.QueryAsync<JournalLineDao.JournalLineDao>(lineSql, {| VoucherNumbers = voucherNumbers |> List.toArray |})

            // 3. 仕訳貸借明細を一括取得
            let itemSql = """
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
                WHERE "仕訳伝票番号" = ANY(@VoucherNumbers)
                ORDER BY "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分"
            """
            let! itemResults = conn.QueryAsync<JournalLineItemDao.JournalLineItemDao>(itemSql, {| VoucherNumbers = voucherNumbers |> List.toArray |})

            // アイテムを (伝票番号, 行番号) でグループ化
            let itemsByVoucherAndLine =
                itemResults
                |> Seq.map JournalLineItemDao.JournalLineItemDao.toDomain
                |> Seq.groupBy (fun item -> (item.VoucherNumber.Number, item.LineNumber))
                |> Map.ofSeq

            // 明細を伝票番号でグループ化
            let linesByVoucher =
                lineResults
                |> Seq.map (fun lineDao ->
                    let baseLine = JournalLineDao.JournalLineDao.toDomain lineDao
                    let key = (baseLine.VoucherNumber.Number, baseLine.LineNumber)
                    let items = itemsByVoucherAndLine |> Map.tryFind key |> Option.defaultValue Seq.empty |> Seq.toList
                    (lineDao.VoucherNumber, { baseLine with Items = items }))
                |> Seq.groupBy fst
                |> Seq.map (fun (vn, lines) -> (vn, lines |> Seq.map snd |> Seq.toList))
                |> Map.ofSeq

            // 集約を組み立て
            return journals
                |> List.map (fun jDao ->
                    let journal = JournalDao.JournalDao.toDomain jDao
                    let lines = linesByVoucher |> Map.tryFind jDao.VoucherNumber |> Option.defaultValue []
                    { journal with Lines = lines })
    }

/// 仕訳を更新（ヘッダーのみ）
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

        let parameters = JournalDao.JournalDao.fromDomain journal
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
