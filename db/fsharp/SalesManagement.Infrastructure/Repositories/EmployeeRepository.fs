module SalesManagement.Infrastructure.Repositories.EmployeeRepository

open System
open System.Threading.Tasks
open Dapper
open Npgsql
open SalesManagement.Domain.Models

/// <summary>
/// 社員を登録
/// </summary>
let insertAsync (connectionString: string) (employee: Employee) : Task<unit> =
    task {
        let sql = """
            INSERT INTO 社員マスタ (
                社員コード, 社員名, 社員名カナ, 性別, 生年月日, 入社年月日, 部門コード, 役職コード,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @EmployeeCode, @EmployeeName, @EmployeeNameKana, @Gender,
                @BirthDate, @JoinDate, @DepartmentCode, @PositionCode,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )"""

        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, employee)
        return ()
    }

/// <summary>
/// 社員を更新
/// </summary>
let updateAsync (connectionString: string) (employee: Employee) : Task<unit> =
    task {
        let sql = """
            UPDATE 社員マスタ
            SET 社員名 = @EmployeeName,
                社員名カナ = @EmployeeNameKana,
                性別 = @Gender,
                生年月日 = @BirthDate,
                入社年月日 = @JoinDate,
                部門コード = @DepartmentCode,
                役職コード = @PositionCode,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 社員コード = @EmployeeCode"""

        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, employee)
        return ()
    }

/// <summary>
/// 社員を削除
/// </summary>
let deleteAsync (connectionString: string) (employeeCode: string) : Task<unit> =
    task {
        let sql = """
            DELETE FROM 社員マスタ
            WHERE 社員コード = @EmployeeCode"""

        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, {| EmployeeCode = employeeCode |})
        return ()
    }

/// <summary>
/// 社員コードで検索
/// </summary>
let findByIdAsync (connectionString: string) (employeeCode: string) : Task<Employee option> =
    task {
        let sql = """
            SELECT
                社員コード AS EmployeeCode,
                社員名 AS EmployeeName,
                社員名カナ AS EmployeeNameKana,
                性別 AS Gender,
                生年月日 AS BirthDate,
                入社年月日 AS JoinDate,
                部門コード AS DepartmentCode,
                役職コード AS PositionCode,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 社員マスタ
            WHERE 社員コード = @EmployeeCode"""

        use connection = new NpgsqlConnection(connectionString)
        let! result = connection.QuerySingleOrDefaultAsync<Employee>(sql, {| EmployeeCode = employeeCode |})
        return if isNull (box result) then None else Some result
    }

/// <summary>
/// すべての社員を取得
/// </summary>
let findAllAsync (connectionString: string) : Task<seq<Employee>> =
    task {
        let sql = """
            SELECT
                社員コード AS EmployeeCode,
                社員名 AS EmployeeName,
                社員名カナ AS EmployeeNameKana,
                性別 AS Gender,
                生年月日 AS BirthDate,
                入社年月日 AS JoinDate,
                部門コード AS DepartmentCode,
                役職コード AS PositionCode,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 社員マスタ
            ORDER BY 社員コード"""

        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<Employee>(sql)
        return results
    }

/// <summary>
/// 部門コードで社員を検索
/// </summary>
let findByDepartmentCodeAsync (connectionString: string) (departmentCode: string) : Task<seq<Employee>> =
    task {
        let sql = """
            SELECT
                社員コード AS EmployeeCode,
                社員名 AS EmployeeName,
                社員名カナ AS EmployeeNameKana,
                性別 AS Gender,
                生年月日 AS BirthDate,
                入社年月日 AS JoinDate,
                部門コード AS DepartmentCode,
                役職コード AS PositionCode,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 社員マスタ
            WHERE 部門コード = @DepartmentCode
            ORDER BY 社員コード"""

        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<Employee>(sql, {| DepartmentCode = departmentCode |})
        return results
    }

/// <summary>
/// 部門に所属する社員を取得（JOIN）
/// </summary>
type EmployeeWithDepartment = {
    社員コード: string
    社員名: string
    部門コード: string
    部門名: string
}

let findEmployeesWithDepartmentAsync (connectionString: string) (departmentCode: string) : Task<seq<EmployeeWithDepartment>> =
    task {
        let sql = """
            SELECT
                e.社員コード,
                e.社員名,
                e.部門コード,
                d.部門名
            FROM 社員マスタ e
            INNER JOIN 部門マスタ d ON e.部門コード = d.部門コード
            WHERE e.部門コード = @DepartmentCode
            ORDER BY e.社員コード"""

        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<EmployeeWithDepartment>(sql, {| DepartmentCode = departmentCode |})
        return results
    }
