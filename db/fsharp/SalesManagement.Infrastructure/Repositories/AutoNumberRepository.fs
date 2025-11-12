module SalesManagement.Infrastructure.Repositories.AutoNumberRepository

open System
open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let findByIdAsync (connectionString: string) (slipType: string) (yearMonth: string) : Task<AutoNumber option> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            SELECT
                伝票種別コード AS SlipType,
                年月 AS YearMonth,
                最終伝票番号 AS LastSlipNo,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 自動採番マスタ
            WHERE 伝票種別コード = @SlipType
              AND 年月 = @YearMonth
        """
        let! result = connection.QuerySingleOrDefaultAsync<AutoNumber>(sql,
            {| SlipType = slipType; YearMonth = yearMonth |})
        return if isNull (box result) then None else Some result
    }

let findByIdForUpdateAsync (connectionString: string) (connection: NpgsqlConnection) (transaction: NpgsqlTransaction) (slipType: string) (yearMonth: string) : Task<AutoNumber option> =
    task {
        let sql = """
            SELECT
                伝票種別コード AS SlipType,
                年月 AS YearMonth,
                最終伝票番号 AS LastSlipNo,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 自動採番マスタ
            WHERE 伝票種別コード = @SlipType
              AND 年月 = @YearMonth
            FOR UPDATE
        """
        let! result = connection.QuerySingleOrDefaultAsync<AutoNumber>(sql,
            {| SlipType = slipType; YearMonth = yearMonth |}, transaction)
        return if isNull (box result) then None else Some result
    }

let findAllAsync (connectionString: string) : Task<AutoNumber seq> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            SELECT
                伝票種別コード AS SlipType,
                年月 AS YearMonth,
                最終伝票番号 AS LastSlipNo,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 自動採番マスタ
            ORDER BY 伝票種別コード, 年月
        """
        let! results = connection.QueryAsync<AutoNumber>(sql)
        return results
    }

let insertAsync (connectionString: string) (autoNumber: AutoNumber) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            INSERT INTO 自動採番マスタ (
                伝票種別コード, 年月, 最終伝票番号,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @SlipType, @YearMonth, @LastSlipNo,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """
        let! _ = connection.ExecuteAsync(sql, autoNumber)
        return ()
    }

let private insertWithConnectionAsync (connection: NpgsqlConnection) (transaction: NpgsqlTransaction) (autoNumber: AutoNumber) : Task<unit> =
    task {
        let sql = """
            INSERT INTO 自動採番マスタ (
                伝票種別コード, 年月, 最終伝票番号,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @SlipType, @YearMonth, @LastSlipNo,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """
        let! _ = connection.ExecuteAsync(sql, autoNumber, transaction)
        return ()
    }

let updateAsync (connectionString: string) (autoNumber: AutoNumber) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            UPDATE 自動採番マスタ SET
                最終伝票番号 = @LastSlipNo,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 伝票種別コード = @SlipType
              AND 年月 = @YearMonth
        """
        let! _ = connection.ExecuteAsync(sql, autoNumber)
        return ()
    }

let private updateWithConnectionAsync (connection: NpgsqlConnection) (transaction: NpgsqlTransaction) (autoNumber: AutoNumber) : Task<unit> =
    task {
        let sql = """
            UPDATE 自動採番マスタ SET
                最終伝票番号 = @LastSlipNo,
                更新日時 = CURRENT_TIMESTAMP
            WHERE 伝票種別コード = @SlipType
              AND 年月 = @YearMonth
        """
        let! _ = connection.ExecuteAsync(sql, autoNumber, transaction)
        return ()
    }

let incrementLastSlipNoAsync (connectionString: string) (slipType: string) (yearMonth: string) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            UPDATE 自動採番マスタ SET
                最終伝票番号 = 最終伝票番号 + 1,
                更新日時 = CURRENT_TIMESTAMP
            WHERE 伝票種別コード = @SlipType
              AND 年月 = @YearMonth
        """
        let! _ = connection.ExecuteAsync(sql, {| SlipType = slipType; YearMonth = yearMonth |})
        return ()
    }

/// 伝票番号を生成（トランザクション付き、FOR UPDATE使用）
let generateSlipNoAsync (connectionString: string) (slipType: string) : Task<string> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        do! connection.OpenAsync()

        use transaction = connection.BeginTransaction()

        try
            let yearMonth = DateTime.Now.ToString("yyyyMM")

            // FOR UPDATEで排他ロック
            let! resultOpt = findByIdForUpdateAsync connectionString connection transaction slipType yearMonth

            let! autoNumber =
                task {
                    match resultOpt with
                    | None ->
                        // 新しい年月のレコードを作成
                        let newAutoNumber = {
                            SlipType = slipType
                            YearMonth = yearMonth
                            LastSlipNo = 1
                            CreatedAt = DateTime.Now
                            CreatedBy = Some "system"
                            UpdatedAt = DateTime.Now
                            UpdatedBy = Some "system"
                        }

                        do! insertWithConnectionAsync connection transaction newAutoNumber
                        return newAutoNumber
                    | Some result ->
                        // 番号をインクリメント
                        let updatedAutoNumber = { result with LastSlipNo = result.LastSlipNo + 1 }

                        do! updateWithConnectionAsync connection transaction updatedAutoNumber
                        return updatedAutoNumber
                }

            do! transaction.CommitAsync()

            // 伝票番号を生成（例: OR20250100001）
            let slipNo = sprintf "%s%s%05d" slipType yearMonth autoNumber.LastSlipNo
            return slipNo
        with
        | ex ->
            do! transaction.RollbackAsync()
            return raise ex
    }

let deleteAsync (connectionString: string) (slipType: string) (yearMonth: string) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            DELETE FROM 自動採番マスタ
            WHERE 伝票種別コード = @SlipType
              AND 年月 = @YearMonth
        """
        let! _ = connection.ExecuteAsync(sql, {| SlipType = slipType; YearMonth = yearMonth |})
        return ()
    }
