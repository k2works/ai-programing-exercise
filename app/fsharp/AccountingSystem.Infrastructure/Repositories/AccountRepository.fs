module AccountingSystem.Infrastructure.Repositories.AccountRepository

open AccountingSystem.Domain
open Dapper
open Npgsql
open System
open System.Reflection

/// <summary>
/// 勘定科目エンティティレコード（リポジトリ用）
/// </summary>
type AccountEntity = {
    AccountId: int option
    AccountCode: string
    AccountName: string
    AccountNameKana: string option
    AccountType: string
    IsSummaryAccount: bool
    BsplType: string option
    TransactionElementType: string option
    ExpenseType: string option
    DisplayOrder: int
    IsAggregationTarget: bool
    TaxCode: string option
    Balance: decimal
    CreatedAt: DateTime option
    UpdatedAt: DateTime option
}

/// AccountEntity のファクトリ関数
module AccountEntity =
    let create accountCode accountName accountType isSummaryAccount =
        {
            AccountId = None
            AccountCode = accountCode
            AccountName = accountName
            AccountNameKana = None
            AccountType = accountType
            IsSummaryAccount = isSummaryAccount
            BsplType = None
            TransactionElementType = None
            ExpenseType = None
            DisplayOrder = 0
            IsAggregationTarget = true
            TaxCode = None
            Balance = 0m
            CreatedAt = None
            UpdatedAt = None
        }

// Dapper カラムマッピング設定
let private initializeMapping() =
    SqlMapper.SetTypeMap(
        typeof<AccountEntity>,
        CustomPropertyTypeMap(
            typeof<AccountEntity>,
            Func<Type, string, PropertyInfo>(fun typ columnName ->
                match columnName with
                | "勘定科目ID" -> typ.GetProperty(nameof Unchecked.defaultof<AccountEntity>.AccountId)
                | "勘定科目コード" -> typ.GetProperty(nameof Unchecked.defaultof<AccountEntity>.AccountCode)
                | "勘定科目名" -> typ.GetProperty(nameof Unchecked.defaultof<AccountEntity>.AccountName)
                | "勘定科目カナ" -> typ.GetProperty(nameof Unchecked.defaultof<AccountEntity>.AccountNameKana)
                | "勘定科目種別" -> typ.GetProperty(nameof Unchecked.defaultof<AccountEntity>.AccountType)
                | "合計科目" -> typ.GetProperty(nameof Unchecked.defaultof<AccountEntity>.IsSummaryAccount)
                | "BSPL区分" -> typ.GetProperty(nameof Unchecked.defaultof<AccountEntity>.BsplType)
                | "取引要素区分" -> typ.GetProperty(nameof Unchecked.defaultof<AccountEntity>.TransactionElementType)
                | "費用区分" -> typ.GetProperty(nameof Unchecked.defaultof<AccountEntity>.ExpenseType)
                | "表示順序" -> typ.GetProperty(nameof Unchecked.defaultof<AccountEntity>.DisplayOrder)
                | "集計対象" -> typ.GetProperty(nameof Unchecked.defaultof<AccountEntity>.IsAggregationTarget)
                | "課税取引コード" -> typ.GetProperty(nameof Unchecked.defaultof<AccountEntity>.TaxCode)
                | "残高" -> typ.GetProperty(nameof Unchecked.defaultof<AccountEntity>.Balance)
                | "作成日時" -> typ.GetProperty(nameof Unchecked.defaultof<AccountEntity>.CreatedAt)
                | "更新日時" -> typ.GetProperty(nameof Unchecked.defaultof<AccountEntity>.UpdatedAt)
                | _ -> null
            )
        )
    )

// モジュール初期化時に一度だけマッピング設定を実行
do initializeMapping()

/// 勘定科目を登録
let insertAsync (connectionString: string) (account: AccountEntity) =
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

        use cmd = new NpgsqlCommand(sql, conn)
        cmd.Parameters.AddWithValue("AccountCode", account.AccountCode) |> ignore
        cmd.Parameters.AddWithValue("AccountName", account.AccountName) |> ignore
        cmd.Parameters.AddWithValue("AccountNameKana", (match account.AccountNameKana with Some v -> box v | None -> box DBNull.Value)) |> ignore
        cmd.Parameters.AddWithValue("AccountType", account.AccountType) |> ignore
        cmd.Parameters.AddWithValue("IsSummaryAccount", account.IsSummaryAccount) |> ignore
        cmd.Parameters.AddWithValue("BsplType", (match account.BsplType with Some v -> box v | None -> box DBNull.Value)) |> ignore
        cmd.Parameters.AddWithValue("TransactionElementType", (match account.TransactionElementType with Some v -> box v | None -> box DBNull.Value)) |> ignore
        cmd.Parameters.AddWithValue("ExpenseType", (match account.ExpenseType with Some v -> box v | None -> box DBNull.Value)) |> ignore
        cmd.Parameters.AddWithValue("DisplayOrder", account.DisplayOrder) |> ignore
        cmd.Parameters.AddWithValue("IsAggregationTarget", account.IsAggregationTarget) |> ignore
        cmd.Parameters.AddWithValue("TaxCode", (match account.TaxCode with Some v -> box v | None -> box DBNull.Value)) |> ignore
        cmd.Parameters.AddWithValue("Balance", account.Balance) |> ignore

        let! result = cmd.ExecuteScalarAsync()
        return Convert.ToInt32(result)
    }

/// 勘定科目コードで検索
let findByCodeAsync (connectionString: string) (accountCode: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "勘定科目ID", "勘定科目コード", "勘定科目名", "勘定科目カナ",
                   "勘定科目種別"::text, "合計科目", "BSPL区分", "取引要素区分",
                   "費用区分", "表示順序", "集計対象", "課税取引コード", "残高",
                   "作成日時", "更新日時"
            FROM "勘定科目マスタ"
            WHERE "勘定科目コード" = @AccountCode
        """

        use cmd = new NpgsqlCommand(sql, conn)
        cmd.Parameters.AddWithValue("AccountCode", accountCode) |> ignore
        use! reader = cmd.ExecuteReaderAsync()

        let! hasData = reader.ReadAsync()
        if hasData then
            let getOptionalString ordinal =
                if reader.IsDBNull(ordinal) then None
                else Some (reader.GetString(ordinal))

            let getOptionalInt ordinal =
                if reader.IsDBNull(ordinal) then None
                else Some (reader.GetInt32(ordinal))

            let getOptionalDateTime ordinal =
                if reader.IsDBNull(ordinal) then None
                else Some (reader.GetDateTime(ordinal))

            return Some {
                AccountId = getOptionalInt (reader.GetOrdinal("勘定科目ID"))
                AccountCode = reader.GetString(reader.GetOrdinal("勘定科目コード"))
                AccountName = reader.GetString(reader.GetOrdinal("勘定科目名"))
                AccountNameKana = getOptionalString (reader.GetOrdinal("勘定科目カナ"))
                AccountType = reader.GetString(reader.GetOrdinal("勘定科目種別"))
                IsSummaryAccount = reader.GetBoolean(reader.GetOrdinal("合計科目"))
                BsplType = getOptionalString (reader.GetOrdinal("BSPL区分"))
                TransactionElementType = getOptionalString (reader.GetOrdinal("取引要素区分"))
                ExpenseType = getOptionalString (reader.GetOrdinal("費用区分"))
                DisplayOrder = reader.GetInt32(reader.GetOrdinal("表示順序"))
                IsAggregationTarget = reader.GetBoolean(reader.GetOrdinal("集計対象"))
                TaxCode = getOptionalString (reader.GetOrdinal("課税取引コード"))
                Balance = reader.GetDecimal(reader.GetOrdinal("残高"))
                CreatedAt = getOptionalDateTime (reader.GetOrdinal("作成日時"))
                UpdatedAt = getOptionalDateTime (reader.GetOrdinal("更新日時"))
            }
        else
            return None
    }

/// 勘定科目IDで検索
let findByIdAsync (connectionString: string) (accountId: int) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "勘定科目ID", "勘定科目コード", "勘定科目名", "勘定科目カナ",
                   "勘定科目種別"::text, "合計科目", "BSPL区分", "取引要素区分",
                   "費用区分", "表示順序", "集計対象", "課税取引コード", "残高",
                   "作成日時", "更新日時"
            FROM "勘定科目マスタ"
            WHERE "勘定科目ID" = @AccountId
        """

        use cmd = new NpgsqlCommand(sql, conn)
        cmd.Parameters.AddWithValue("AccountId", accountId) |> ignore
        use! reader = cmd.ExecuteReaderAsync()

        let! hasData = reader.ReadAsync()
        if hasData then
            let getOptionalString ordinal =
                if reader.IsDBNull(ordinal) then None
                else Some (reader.GetString(ordinal))

            let getOptionalInt ordinal =
                if reader.IsDBNull(ordinal) then None
                else Some (reader.GetInt32(ordinal))

            let getOptionalDateTime ordinal =
                if reader.IsDBNull(ordinal) then None
                else Some (reader.GetDateTime(ordinal))

            return Some {
                AccountId = getOptionalInt (reader.GetOrdinal("勘定科目ID"))
                AccountCode = reader.GetString(reader.GetOrdinal("勘定科目コード"))
                AccountName = reader.GetString(reader.GetOrdinal("勘定科目名"))
                AccountNameKana = getOptionalString (reader.GetOrdinal("勘定科目カナ"))
                AccountType = reader.GetString(reader.GetOrdinal("勘定科目種別"))
                IsSummaryAccount = reader.GetBoolean(reader.GetOrdinal("合計科目"))
                BsplType = getOptionalString (reader.GetOrdinal("BSPL区分"))
                TransactionElementType = getOptionalString (reader.GetOrdinal("取引要素区分"))
                ExpenseType = getOptionalString (reader.GetOrdinal("費用区分"))
                DisplayOrder = reader.GetInt32(reader.GetOrdinal("表示順序"))
                IsAggregationTarget = reader.GetBoolean(reader.GetOrdinal("集計対象"))
                TaxCode = getOptionalString (reader.GetOrdinal("課税取引コード"))
                Balance = reader.GetDecimal(reader.GetOrdinal("残高"))
                CreatedAt = getOptionalDateTime (reader.GetOrdinal("作成日時"))
                UpdatedAt = getOptionalDateTime (reader.GetOrdinal("更新日時"))
            }
        else
            return None
    }

/// 全ての勘定科目を取得
let findAllAsync (connectionString: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "勘定科目ID", "勘定科目コード", "勘定科目名", "勘定科目カナ",
                   "勘定科目種別"::text, "合計科目", "BSPL区分", "取引要素区分",
                   "費用区分", "表示順序", "集計対象", "課税取引コード", "残高",
                   "作成日時", "更新日時"
            FROM "勘定科目マスタ"
            ORDER BY "勘定科目コード"
        """

        use cmd = new NpgsqlCommand(sql, conn)
        use! reader = cmd.ExecuteReaderAsync()

        let results = ResizeArray<AccountEntity>()
        while reader.Read() do
            let getOptionalString ordinal =
                if reader.IsDBNull(ordinal) then None
                else Some (reader.GetString(ordinal))

            let getOptionalInt ordinal =
                if reader.IsDBNull(ordinal) then None
                else Some (reader.GetInt32(ordinal))

            let getOptionalDateTime ordinal =
                if reader.IsDBNull(ordinal) then None
                else Some (reader.GetDateTime(ordinal))

            results.Add({
                AccountId = getOptionalInt (reader.GetOrdinal("勘定科目ID"))
                AccountCode = reader.GetString(reader.GetOrdinal("勘定科目コード"))
                AccountName = reader.GetString(reader.GetOrdinal("勘定科目名"))
                AccountNameKana = getOptionalString (reader.GetOrdinal("勘定科目カナ"))
                AccountType = reader.GetString(reader.GetOrdinal("勘定科目種別"))
                IsSummaryAccount = reader.GetBoolean(reader.GetOrdinal("合計科目"))
                BsplType = getOptionalString (reader.GetOrdinal("BSPL区分"))
                TransactionElementType = getOptionalString (reader.GetOrdinal("取引要素区分"))
                ExpenseType = getOptionalString (reader.GetOrdinal("費用区分"))
                DisplayOrder = reader.GetInt32(reader.GetOrdinal("表示順序"))
                IsAggregationTarget = reader.GetBoolean(reader.GetOrdinal("集計対象"))
                TaxCode = getOptionalString (reader.GetOrdinal("課税取引コード"))
                Balance = reader.GetDecimal(reader.GetOrdinal("残高"))
                CreatedAt = getOptionalDateTime (reader.GetOrdinal("作成日時"))
                UpdatedAt = getOptionalDateTime (reader.GetOrdinal("更新日時"))
            })

        return results |> Seq.toList
    }

/// 勘定科目種別で検索
let findByTypeAsync (connectionString: string) (accountType: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "勘定科目ID", "勘定科目コード", "勘定科目名", "勘定科目カナ",
                   "勘定科目種別"::text, "合計科目", "BSPL区分", "取引要素区分",
                   "費用区分", "表示順序", "集計対象", "課税取引コード", "残高",
                   "作成日時", "更新日時"
            FROM "勘定科目マスタ"
            WHERE "勘定科目種別" = @AccountType::account_type
            ORDER BY "表示順序", "勘定科目コード"
        """

        use cmd = new NpgsqlCommand(sql, conn)
        cmd.Parameters.AddWithValue("AccountType", accountType) |> ignore
        use! reader = cmd.ExecuteReaderAsync()

        let results = ResizeArray<AccountEntity>()
        while reader.Read() do
            let getOptionalString ordinal =
                if reader.IsDBNull(ordinal) then None
                else Some (reader.GetString(ordinal))

            let getOptionalInt ordinal =
                if reader.IsDBNull(ordinal) then None
                else Some (reader.GetInt32(ordinal))

            let getOptionalDateTime ordinal =
                if reader.IsDBNull(ordinal) then None
                else Some (reader.GetDateTime(ordinal))

            results.Add({
                AccountId = getOptionalInt (reader.GetOrdinal("勘定科目ID"))
                AccountCode = reader.GetString(reader.GetOrdinal("勘定科目コード"))
                AccountName = reader.GetString(reader.GetOrdinal("勘定科目名"))
                AccountNameKana = getOptionalString (reader.GetOrdinal("勘定科目カナ"))
                AccountType = reader.GetString(reader.GetOrdinal("勘定科目種別"))
                IsSummaryAccount = reader.GetBoolean(reader.GetOrdinal("合計科目"))
                BsplType = getOptionalString (reader.GetOrdinal("BSPL区分"))
                TransactionElementType = getOptionalString (reader.GetOrdinal("取引要素区分"))
                ExpenseType = getOptionalString (reader.GetOrdinal("費用区分"))
                DisplayOrder = reader.GetInt32(reader.GetOrdinal("表示順序"))
                IsAggregationTarget = reader.GetBoolean(reader.GetOrdinal("集計対象"))
                TaxCode = getOptionalString (reader.GetOrdinal("課税取引コード"))
                Balance = reader.GetDecimal(reader.GetOrdinal("残高"))
                CreatedAt = getOptionalDateTime (reader.GetOrdinal("作成日時"))
                UpdatedAt = getOptionalDateTime (reader.GetOrdinal("更新日時"))
            })

        return results |> Seq.toList
    }

/// 合計科目を取得
let findSummaryAccountsAsync (connectionString: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "勘定科目ID", "勘定科目コード", "勘定科目名", "勘定科目カナ",
                   "勘定科目種別"::text, "合計科目", "BSPL区分", "取引要素区分",
                   "費用区分", "表示順序", "集計対象", "課税取引コード", "残高",
                   "作成日時", "更新日時"
            FROM "勘定科目マスタ"
            WHERE "合計科目" = true
            ORDER BY "表示順序", "勘定科目コード"
        """

        use cmd = new NpgsqlCommand(sql, conn)
        use! reader = cmd.ExecuteReaderAsync()

        let results = ResizeArray<AccountEntity>()
        while reader.Read() do
            let getOptionalString ordinal =
                if reader.IsDBNull(ordinal) then None
                else Some (reader.GetString(ordinal))

            let getOptionalInt ordinal =
                if reader.IsDBNull(ordinal) then None
                else Some (reader.GetInt32(ordinal))

            let getOptionalDateTime ordinal =
                if reader.IsDBNull(ordinal) then None
                else Some (reader.GetDateTime(ordinal))

            results.Add({
                AccountId = getOptionalInt (reader.GetOrdinal("勘定科目ID"))
                AccountCode = reader.GetString(reader.GetOrdinal("勘定科目コード"))
                AccountName = reader.GetString(reader.GetOrdinal("勘定科目名"))
                AccountNameKana = getOptionalString (reader.GetOrdinal("勘定科目カナ"))
                AccountType = reader.GetString(reader.GetOrdinal("勘定科目種別"))
                IsSummaryAccount = reader.GetBoolean(reader.GetOrdinal("合計科目"))
                BsplType = getOptionalString (reader.GetOrdinal("BSPL区分"))
                TransactionElementType = getOptionalString (reader.GetOrdinal("取引要素区分"))
                ExpenseType = getOptionalString (reader.GetOrdinal("費用区分"))
                DisplayOrder = reader.GetInt32(reader.GetOrdinal("表示順序"))
                IsAggregationTarget = reader.GetBoolean(reader.GetOrdinal("集計対象"))
                TaxCode = getOptionalString (reader.GetOrdinal("課税取引コード"))
                Balance = reader.GetDecimal(reader.GetOrdinal("残高"))
                CreatedAt = getOptionalDateTime (reader.GetOrdinal("作成日時"))
                UpdatedAt = getOptionalDateTime (reader.GetOrdinal("更新日時"))
            })

        return results |> Seq.toList
    }

/// 明細科目を取得
let findDetailAccountsAsync (connectionString: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "勘定科目ID", "勘定科目コード", "勘定科目名", "勘定科目カナ",
                   "勘定科目種別"::text, "合計科目", "BSPL区分", "取引要素区分",
                   "費用区分", "表示順序", "集計対象", "課税取引コード", "残高",
                   "作成日時", "更新日時"
            FROM "勘定科目マスタ"
            WHERE "合計科目" = false
            ORDER BY "表示順序", "勘定科目コード"
        """

        use cmd = new NpgsqlCommand(sql, conn)
        use! reader = cmd.ExecuteReaderAsync()

        let results = ResizeArray<AccountEntity>()
        while reader.Read() do
            let getOptionalString ordinal =
                if reader.IsDBNull(ordinal) then None
                else Some (reader.GetString(ordinal))

            let getOptionalInt ordinal =
                if reader.IsDBNull(ordinal) then None
                else Some (reader.GetInt32(ordinal))

            let getOptionalDateTime ordinal =
                if reader.IsDBNull(ordinal) then None
                else Some (reader.GetDateTime(ordinal))

            results.Add({
                AccountId = getOptionalInt (reader.GetOrdinal("勘定科目ID"))
                AccountCode = reader.GetString(reader.GetOrdinal("勘定科目コード"))
                AccountName = reader.GetString(reader.GetOrdinal("勘定科目名"))
                AccountNameKana = getOptionalString (reader.GetOrdinal("勘定科目カナ"))
                AccountType = reader.GetString(reader.GetOrdinal("勘定科目種別"))
                IsSummaryAccount = reader.GetBoolean(reader.GetOrdinal("合計科目"))
                BsplType = getOptionalString (reader.GetOrdinal("BSPL区分"))
                TransactionElementType = getOptionalString (reader.GetOrdinal("取引要素区分"))
                ExpenseType = getOptionalString (reader.GetOrdinal("費用区分"))
                DisplayOrder = reader.GetInt32(reader.GetOrdinal("表示順序"))
                IsAggregationTarget = reader.GetBoolean(reader.GetOrdinal("集計対象"))
                TaxCode = getOptionalString (reader.GetOrdinal("課税取引コード"))
                Balance = reader.GetDecimal(reader.GetOrdinal("残高"))
                CreatedAt = getOptionalDateTime (reader.GetOrdinal("作成日時"))
                UpdatedAt = getOptionalDateTime (reader.GetOrdinal("更新日時"))
            })

        return results |> Seq.toList
    }

/// 勘定科目を更新
let updateAsync (connectionString: string) (account: AccountEntity) =
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

        use cmd = new NpgsqlCommand(sql, conn)
        cmd.Parameters.AddWithValue("AccountCode", account.AccountCode) |> ignore
        cmd.Parameters.AddWithValue("AccountName", account.AccountName) |> ignore
        cmd.Parameters.AddWithValue("AccountNameKana", (match account.AccountNameKana with Some v -> box v | None -> box DBNull.Value)) |> ignore
        cmd.Parameters.AddWithValue("AccountType", account.AccountType) |> ignore
        cmd.Parameters.AddWithValue("IsSummaryAccount", account.IsSummaryAccount) |> ignore
        cmd.Parameters.AddWithValue("BsplType", (match account.BsplType with Some v -> box v | None -> box DBNull.Value)) |> ignore
        cmd.Parameters.AddWithValue("TransactionElementType", (match account.TransactionElementType with Some v -> box v | None -> box DBNull.Value)) |> ignore
        cmd.Parameters.AddWithValue("ExpenseType", (match account.ExpenseType with Some v -> box v | None -> box DBNull.Value)) |> ignore
        cmd.Parameters.AddWithValue("DisplayOrder", account.DisplayOrder) |> ignore
        cmd.Parameters.AddWithValue("IsAggregationTarget", account.IsAggregationTarget) |> ignore
        cmd.Parameters.AddWithValue("TaxCode", (match account.TaxCode with Some v -> box v | None -> box DBNull.Value)) |> ignore
        cmd.Parameters.AddWithValue("Balance", account.Balance) |> ignore

        let! rowsAffected = cmd.ExecuteNonQueryAsync()
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

        use cmd = new NpgsqlCommand(sql, conn)
        cmd.Parameters.AddWithValue("AccountCode", accountCode) |> ignore
        cmd.Parameters.AddWithValue("Balance", balance) |> ignore

        let! rowsAffected = cmd.ExecuteNonQueryAsync()
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

        use cmd = new NpgsqlCommand(sql, conn)
        cmd.Parameters.AddWithValue("AccountCode", accountCode) |> ignore

        let! rowsAffected = cmd.ExecuteNonQueryAsync()
        return rowsAffected
    }
