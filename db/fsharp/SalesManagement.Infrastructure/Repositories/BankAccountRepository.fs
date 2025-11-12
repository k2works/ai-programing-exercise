module SalesManagement.Infrastructure.Repositories.BankAccountRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let findByIdAsync (connectionString: string) (accountCode: string) : Task<BankAccount option> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            SELECT
                口座コード AS AccountCode,
                口座名 AS AccountName,
                銀行名 AS BankName,
                支店名 AS BranchName,
                口座番号 AS AccountNumber,
                口座種別 AS AccountType,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 入金口座マスタ
            WHERE 口座コード = @AccountCode
        """
        let! result = connection.QuerySingleOrDefaultAsync<BankAccount>(sql, {| AccountCode = accountCode |})
        return if isNull (box result) then None else Some result
    }

let findAllAsync (connectionString: string) : Task<BankAccount seq> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            SELECT
                口座コード AS AccountCode,
                口座名 AS AccountName,
                銀行名 AS BankName,
                支店名 AS BranchName,
                口座番号 AS AccountNumber,
                口座種別 AS AccountType,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 入金口座マスタ
            ORDER BY 口座コード
        """
        let! results = connection.QueryAsync<BankAccount>(sql)
        return results
    }

let insertAsync (connectionString: string) (bankAccount: BankAccount) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            INSERT INTO 入金口座マスタ (
                口座コード, 口座名, 銀行名, 支店名, 口座番号, 口座種別,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @AccountCode, @AccountName, @BankName, @BranchName,
                @AccountNumber, @AccountType,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """
        let! _ = connection.ExecuteAsync(sql, bankAccount)
        return ()
    }

let updateAsync (connectionString: string) (bankAccount: BankAccount) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            UPDATE 入金口座マスタ SET
                口座名 = @AccountName,
                銀行名 = @BankName,
                支店名 = @BranchName,
                口座番号 = @AccountNumber,
                口座種別 = @AccountType,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 口座コード = @AccountCode
        """
        let! _ = connection.ExecuteAsync(sql, bankAccount)
        return ()
    }

let deleteAsync (connectionString: string) (accountCode: string) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = "DELETE FROM 入金口座マスタ WHERE 口座コード = @AccountCode"
        let! _ = connection.ExecuteAsync(sql, {| AccountCode = accountCode |})
        return ()
    }
