module SalesManagement.Infrastructure.Repositories.DepartmentRepository

open System
open System.Threading.Tasks
open Dapper
open Npgsql
open SalesManagement.Domain.Models

/// <summary>
/// 部門を登録
/// </summary>
let insertAsync (connectionString: string) (department: Department) : Task<unit> =
    task {
        let sql = """
            INSERT INTO 部門マスタ (
                部門コード, 開始日, 終了日, 部門名, 組織階層, 部門パス, 最下層区分, 伝票入力可否,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @DepartmentCode, @StartDate, @EndDate, @DepartmentName,
                @OrganizationLevel, @DepartmentPath, @LowestLevelFlag, @SlipInputFlag,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )"""

        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, department)
        return ()
    }

/// <summary>
/// 部門を更新
/// </summary>
let updateAsync (connectionString: string) (department: Department) : Task<unit> =
    task {
        let sql = """
            UPDATE 部門マスタ
            SET 部門名 = @DepartmentName,
                組織階層 = @OrganizationLevel,
                部門パス = @DepartmentPath,
                最下層区分 = @LowestLevelFlag,
                伝票入力可否 = @SlipInputFlag,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 部門コード = @DepartmentCode
              AND 開始日 = @StartDate"""

        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, department)
        return ()
    }

/// <summary>
/// 部門を削除
/// </summary>
let deleteAsync (connectionString: string) (departmentCode: string) (startDate: DateTime) : Task<unit> =
    task {
        let sql = """
            DELETE FROM 部門マスタ
            WHERE 部門コード = @DepartmentCode
              AND 開始日 = @StartDate"""

        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, {| DepartmentCode = departmentCode; StartDate = startDate |})
        return ()
    }

/// <summary>
/// 主キーで部門を検索
/// </summary>
let findByIdAsync (connectionString: string) (departmentCode: string) (startDate: DateTime) : Task<Department option> =
    task {
        let sql = """
            SELECT
                部門コード AS DepartmentCode,
                開始日 AS StartDate,
                終了日 AS EndDate,
                部門名 AS DepartmentName,
                組織階層 AS OrganizationLevel,
                部門パス AS DepartmentPath,
                最下層区分 AS LowestLevelFlag,
                伝票入力可否 AS SlipInputFlag,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 部門マスタ
            WHERE 部門コード = @DepartmentCode
              AND 開始日 = @StartDate"""

        use connection = new NpgsqlConnection(connectionString)
        let! result = connection.QuerySingleOrDefaultAsync<Department>(sql, {| DepartmentCode = departmentCode; StartDate = startDate |})
        return if isNull (box result) then None else Some result
    }

/// <summary>
/// すべての部門を取得
/// </summary>
let findAllAsync (connectionString: string) : Task<seq<Department>> =
    task {
        let sql = """
            SELECT
                部門コード AS DepartmentCode,
                開始日 AS StartDate,
                終了日 AS EndDate,
                部門名 AS DepartmentName,
                組織階層 AS OrganizationLevel,
                部門パス AS DepartmentPath,
                最下層区分 AS LowestLevelFlag,
                伝票入力可否 AS SlipInputFlag,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 部門マスタ
            ORDER BY 組織階層, 部門コード, 開始日"""

        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<Department>(sql)
        return results
    }

/// <summary>
/// 部門コードで検索（履歴含む）
/// </summary>
let findByDepartmentCodeAsync (connectionString: string) (departmentCode: string) : Task<seq<Department>> =
    task {
        let sql = """
            SELECT
                部門コード AS DepartmentCode,
                開始日 AS StartDate,
                終了日 AS EndDate,
                部門名 AS DepartmentName,
                組織階層 AS OrganizationLevel,
                部門パス AS DepartmentPath,
                最下層区分 AS LowestLevelFlag,
                伝票入力可否 AS SlipInputFlag,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 部門マスタ
            WHERE 部門コード = @DepartmentCode
            ORDER BY 開始日"""

        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<Department>(sql, {| DepartmentCode = departmentCode |})
        return results
    }
