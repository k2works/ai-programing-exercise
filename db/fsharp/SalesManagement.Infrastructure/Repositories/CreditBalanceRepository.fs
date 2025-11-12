module SalesManagement.Infrastructure.Repositories.CreditBalanceRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let findByIdAsync (connectionString: string) (companyCode: string) : Task<CreditBalance option> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            SELECT
                取引先コード AS CompanyCode,
                受注残高 AS OrderBalance,
                債権残高 AS ReceivableBalance,
                債務残高 AS PayableBalance,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 与信残高データ
            WHERE 取引先コード = @CompanyCode
        """
        let! result = connection.QuerySingleOrDefaultAsync<CreditBalance>(sql, {| CompanyCode = companyCode |})
        return if isNull (box result) then None else Some result
    }

let findAllAsync (connectionString: string) : Task<CreditBalance seq> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            SELECT
                取引先コード AS CompanyCode,
                受注残高 AS OrderBalance,
                債権残高 AS ReceivableBalance,
                債務残高 AS PayableBalance,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 与信残高データ
            ORDER BY 取引先コード
        """
        let! results = connection.QueryAsync<CreditBalance>(sql)
        return results
    }

let insertAsync (connectionString: string) (creditBalance: CreditBalance) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            INSERT INTO 与信残高データ (
                取引先コード, 受注残高, 債権残高, 債務残高,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @CompanyCode, @OrderBalance, @ReceivableBalance, @PayableBalance,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """
        let! _ = connection.ExecuteAsync(sql, creditBalance)
        return ()
    }

let updateAsync (connectionString: string) (creditBalance: CreditBalance) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            UPDATE 与信残高データ SET
                受注残高 = @OrderBalance,
                債権残高 = @ReceivableBalance,
                債務残高 = @PayableBalance,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 取引先コード = @CompanyCode
        """
        let! _ = connection.ExecuteAsync(sql, creditBalance)
        return ()
    }

/// 受注残高を加算・減算
let updateOrderBalanceAsync (connectionString: string) (companyCode: string) (amount: decimal) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            UPDATE 与信残高データ SET
                受注残高 = 受注残高 + @Amount,
                更新日時 = CURRENT_TIMESTAMP
            WHERE 取引先コード = @CompanyCode
        """
        let! _ = connection.ExecuteAsync(sql, {| CompanyCode = companyCode; Amount = amount |})
        return ()
    }

/// 債権残高を加算・減算
let updateReceivableBalanceAsync (connectionString: string) (companyCode: string) (amount: decimal) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            UPDATE 与信残高データ SET
                債権残高 = 債権残高 + @Amount,
                更新日時 = CURRENT_TIMESTAMP
            WHERE 取引先コード = @CompanyCode
        """
        let! _ = connection.ExecuteAsync(sql, {| CompanyCode = companyCode; Amount = amount |})
        return ()
    }

/// 債務残高を加算・減算
let updatePayableBalanceAsync (connectionString: string) (companyCode: string) (amount: decimal) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            UPDATE 与信残高データ SET
                債務残高 = 債務残高 + @Amount,
                更新日時 = CURRENT_TIMESTAMP
            WHERE 取引先コード = @CompanyCode
        """
        let! _ = connection.ExecuteAsync(sql, {| CompanyCode = companyCode; Amount = amount |})
        return ()
    }

let deleteAsync (connectionString: string) (companyCode: string) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = "DELETE FROM 与信残高データ WHERE 取引先コード = @CompanyCode"
        let! _ = connection.ExecuteAsync(sql, {| CompanyCode = companyCode |})
        return ()
    }
