module SalesManagement.Infrastructure.Repositories.CompanyGroupRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let insertAsync (connectionString: string) (group: CompanyGroup) : Task<unit> =
    task {
        let sql = """
            INSERT INTO 取引先グループマスタ (
                取引先グループコード, 取引先グループ名,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @CompanyGroupCode, @CompanyGroupName,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, group)
        return ()
    }

let updateAsync (connectionString: string) (group: CompanyGroup) : Task<unit> =
    task {
        let sql = """
            UPDATE 取引先グループマスタ
            SET 取引先グループ名 = @CompanyGroupName,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 取引先グループコード = @CompanyGroupCode"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, group)
        return ()
    }

let deleteAsync (connectionString: string) (groupCode: string) : Task<unit> =
    task {
        let sql = "DELETE FROM 取引先グループマスタ WHERE 取引先グループコード = @GroupCode"
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, {| GroupCode = groupCode |})
        return ()
    }

let findByIdAsync (connectionString: string) (groupCode: string) : Task<CompanyGroup option> =
    task {
        let sql = """
            SELECT 取引先グループコード AS CompanyGroupCode,
                   取引先グループ名 AS CompanyGroupName,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 取引先グループマスタ
            WHERE 取引先グループコード = @GroupCode"""
        use connection = new NpgsqlConnection(connectionString)
        let! result = connection.QuerySingleOrDefaultAsync<CompanyGroup>(sql, {| GroupCode = groupCode |})
        return if isNull (box result) then None else Some result
    }

let findAllAsync (connectionString: string) : Task<seq<CompanyGroup>> =
    task {
        let sql = """
            SELECT 取引先グループコード AS CompanyGroupCode,
                   取引先グループ名 AS CompanyGroupName,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 取引先グループマスタ
            ORDER BY 取引先グループコード"""
        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<CompanyGroup>(sql)
        return results :> seq<CompanyGroup>
    }
