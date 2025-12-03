module AccountingSystem.Infrastructure.Repositories.AccountRepository

open AccountingSystem.Domain.Models
open AccountingSystem.Infrastructure.DAO
open Dapper
open Npgsql

/// 勘定科目を登録
let insertAsync (connectionString: string) (account: Account) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            INSERT INTO "勘定科目マスタ" (
                "勘定科目コード",
                "勘定科目名",
                "勘定科目カナ",
                "勘定科目種別",
                "合計科目",
                "BSPL区分",
                "取引要素区分",
                "費用区分",
                "表示順序",
                "集計対象",
                "課税取引コード",
                "残高",
                "作成日時",
                "更新日時"
            ) VALUES (
                @AccountCode,
                @AccountName,
                @AccountNameKana,
                @AccountType::account_type,
                @IsSummaryAccount,
                @BsplType,
                @TransactionElementType,
                @ExpenseType,
                @DisplayOrder,
                @IsAggregationTarget,
                @TaxCode,
                @Balance,
                CURRENT_TIMESTAMP,
                CURRENT_TIMESTAMP
            )
            RETURNING "勘定科目ID"
        """

        let parameters = AccountDao.fromDomain account
        let! result = conn.ExecuteScalarAsync<int>(sql, parameters)
        return result
    }

/// 勘定科目コードで検索
let findByCodeAsync (connectionString: string) (accountCode: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "勘定科目ID" AS "AccountId",
                   "勘定科目コード" AS "AccountCode",
                   "勘定科目名" AS "AccountName",
                   "勘定科目カナ" AS "AccountNameKana",
                   "勘定科目種別"::text AS "AccountType",
                   "合計科目" AS "IsSummaryAccount",
                   "BSPL区分" AS "BsplType",
                   "取引要素区分" AS "TransactionElementType",
                   "費用区分" AS "ExpenseType",
                   "表示順序" AS "DisplayOrder",
                   "集計対象" AS "IsAggregationTarget",
                   "課税取引コード" AS "TaxCode",
                   "残高" AS "Balance"
            FROM "勘定科目マスタ"
            WHERE "勘定科目コード" = @AccountCode
        """

        let! results = conn.QueryAsync<AccountDao>(sql, {| AccountCode = accountCode |})
        return results |> Seq.tryHead |> Option.map AccountDao.toDomain
    }

/// 勘定科目IDで検索
let findByIdAsync (connectionString: string) (accountId: int) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "勘定科目ID" AS "AccountId",
                   "勘定科目コード" AS "AccountCode",
                   "勘定科目名" AS "AccountName",
                   "勘定科目カナ" AS "AccountNameKana",
                   "勘定科目種別"::text AS "AccountType",
                   "合計科目" AS "IsSummaryAccount",
                   "BSPL区分" AS "BsplType",
                   "取引要素区分" AS "TransactionElementType",
                   "費用区分" AS "ExpenseType",
                   "表示順序" AS "DisplayOrder",
                   "集計対象" AS "IsAggregationTarget",
                   "課税取引コード" AS "TaxCode",
                   "残高" AS "Balance"
            FROM "勘定科目マスタ"
            WHERE "勘定科目ID" = @AccountId
        """

        let! results = conn.QueryAsync<AccountDao>(sql, {| AccountId = accountId |})
        return results |> Seq.tryHead |> Option.map AccountDao.toDomain
    }

/// 全ての勘定科目を取得
let findAllAsync (connectionString: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "勘定科目ID" AS "AccountId",
                   "勘定科目コード" AS "AccountCode",
                   "勘定科目名" AS "AccountName",
                   "勘定科目カナ" AS "AccountNameKana",
                   "勘定科目種別"::text AS "AccountType",
                   "合計科目" AS "IsSummaryAccount",
                   "BSPL区分" AS "BsplType",
                   "取引要素区分" AS "TransactionElementType",
                   "費用区分" AS "ExpenseType",
                   "表示順序" AS "DisplayOrder",
                   "集計対象" AS "IsAggregationTarget",
                   "課税取引コード" AS "TaxCode",
                   "残高" AS "Balance"
            FROM "勘定科目マスタ"
            ORDER BY "勘定科目コード"
        """

        let! results = conn.QueryAsync<AccountDao>(sql)
        return results |> Seq.map AccountDao.toDomain |> Seq.toList
    }

/// 勘定科目種別で検索
let findByTypeAsync (connectionString: string) (accountType: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "勘定科目ID" AS "AccountId",
                   "勘定科目コード" AS "AccountCode",
                   "勘定科目名" AS "AccountName",
                   "勘定科目カナ" AS "AccountNameKana",
                   "勘定科目種別"::text AS "AccountType",
                   "合計科目" AS "IsSummaryAccount",
                   "BSPL区分" AS "BsplType",
                   "取引要素区分" AS "TransactionElementType",
                   "費用区分" AS "ExpenseType",
                   "表示順序" AS "DisplayOrder",
                   "集計対象" AS "IsAggregationTarget",
                   "課税取引コード" AS "TaxCode",
                   "残高" AS "Balance"
            FROM "勘定科目マスタ"
            WHERE "勘定科目種別" = @AccountType::account_type
            ORDER BY "表示順序", "勘定科目コード"
        """

        let! results = conn.QueryAsync<AccountDao>(sql, {| AccountType = accountType |})
        return results |> Seq.map AccountDao.toDomain |> Seq.toList
    }

/// 合計科目を取得
let findSummaryAccountsAsync (connectionString: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "勘定科目ID" AS "AccountId",
                   "勘定科目コード" AS "AccountCode",
                   "勘定科目名" AS "AccountName",
                   "勘定科目カナ" AS "AccountNameKana",
                   "勘定科目種別"::text AS "AccountType",
                   "合計科目" AS "IsSummaryAccount",
                   "BSPL区分" AS "BsplType",
                   "取引要素区分" AS "TransactionElementType",
                   "費用区分" AS "ExpenseType",
                   "表示順序" AS "DisplayOrder",
                   "集計対象" AS "IsAggregationTarget",
                   "課税取引コード" AS "TaxCode",
                   "残高" AS "Balance"
            FROM "勘定科目マスタ"
            WHERE "合計科目" = true
            ORDER BY "表示順序", "勘定科目コード"
        """

        let! results = conn.QueryAsync<AccountDao>(sql)
        return results |> Seq.map AccountDao.toDomain |> Seq.toList
    }

/// 明細科目を取得
let findDetailAccountsAsync (connectionString: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "勘定科目ID" AS "AccountId",
                   "勘定科目コード" AS "AccountCode",
                   "勘定科目名" AS "AccountName",
                   "勘定科目カナ" AS "AccountNameKana",
                   "勘定科目種別"::text AS "AccountType",
                   "合計科目" AS "IsSummaryAccount",
                   "BSPL区分" AS "BsplType",
                   "取引要素区分" AS "TransactionElementType",
                   "費用区分" AS "ExpenseType",
                   "表示順序" AS "DisplayOrder",
                   "集計対象" AS "IsAggregationTarget",
                   "課税取引コード" AS "TaxCode",
                   "残高" AS "Balance"
            FROM "勘定科目マスタ"
            WHERE "合計科目" = false
            ORDER BY "表示順序", "勘定科目コード"
        """

        let! results = conn.QueryAsync<AccountDao>(sql)
        return results |> Seq.map AccountDao.toDomain |> Seq.toList
    }

/// 勘定科目を更新
let updateAsync (connectionString: string) (account: Account) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            UPDATE "勘定科目マスタ"
            SET "勘定科目名" = @AccountName,
                "勘定科目カナ" = @AccountNameKana,
                "勘定科目種別" = @AccountType::account_type,
                "合計科目" = @IsSummaryAccount,
                "BSPL区分" = @BsplType,
                "取引要素区分" = @TransactionElementType,
                "費用区分" = @ExpenseType,
                "表示順序" = @DisplayOrder,
                "集計対象" = @IsAggregationTarget,
                "課税取引コード" = @TaxCode,
                "残高" = @Balance,
                "更新日時" = CURRENT_TIMESTAMP
            WHERE "勘定科目コード" = @AccountCode
        """

        let parameters = AccountDao.fromDomain account
        let! rowsAffected = conn.ExecuteAsync(sql, parameters)
        return rowsAffected
    }

/// 残高を更新
let updateBalanceAsync (connectionString: string) (accountCode: string) (balance: decimal) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            UPDATE "勘定科目マスタ"
            SET "残高" = @Balance,
                "更新日時" = CURRENT_TIMESTAMP
            WHERE "勘定科目コード" = @AccountCode
        """

        let! rowsAffected = conn.ExecuteAsync(sql, {| AccountCode = accountCode; Balance = balance |})
        return rowsAffected
    }

/// 勘定科目を削除
let deleteAsync (connectionString: string) (accountCode: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            DELETE FROM "勘定科目マスタ"
            WHERE "勘定科目コード" = @AccountCode
        """

        let! rowsAffected = conn.ExecuteAsync(sql, {| AccountCode = accountCode |})
        return rowsAffected
    }
