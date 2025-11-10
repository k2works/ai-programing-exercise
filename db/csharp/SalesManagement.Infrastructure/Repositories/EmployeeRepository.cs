using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    /// <summary>
    /// 社員マスタのRepositoryクラス
    /// </summary>
    public class EmployeeRepository
    {
        private readonly string _connectionString;

        public EmployeeRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        /// <summary>
        /// 社員を登録
        /// </summary>
        public async Task InsertAsync(Employee employee)
        {
            const string sql = @"
                INSERT INTO 社員マスタ (
                    社員コード, 社員名, 社員名カナ, 性別, 生年月日, 入社年月日, 部門コード, 役職コード,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @EmployeeCode, @EmployeeName, @EmployeeNameKana, @Gender,
                    @BirthDate, @JoinDate, @DepartmentCode, @PositionCode,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, employee);
        }

        /// <summary>
        /// 社員を更新
        /// </summary>
        public async Task UpdateAsync(Employee employee)
        {
            const string sql = @"
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
                WHERE 社員コード = @EmployeeCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, employee);
        }

        /// <summary>
        /// 社員を削除
        /// </summary>
        public async Task DeleteAsync(string employeeCode)
        {
            const string sql = @"
                DELETE FROM 社員マスタ
                WHERE 社員コード = @EmployeeCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, new { EmployeeCode = employeeCode });
        }

        /// <summary>
        /// 社員コードで検索
        /// </summary>
        public async Task<Employee?> FindByIdAsync(string employeeCode)
        {
            const string sql = @"
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
                WHERE 社員コード = @EmployeeCode";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QuerySingleOrDefaultAsync<Employee>(sql, new { EmployeeCode = employeeCode });
        }

        /// <summary>
        /// すべての社員を取得
        /// </summary>
        public async Task<IEnumerable<Employee>> FindAllAsync()
        {
            const string sql = @"
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
                ORDER BY 社員コード";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Employee>(sql);
        }

        /// <summary>
        /// 部門コードで社員を検索
        /// </summary>
        public async Task<IEnumerable<Employee>> FindByDepartmentCodeAsync(string departmentCode)
        {
            const string sql = @"
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
                ORDER BY 社員コード";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Employee>(sql, new { DepartmentCode = departmentCode });
        }

        /// <summary>
        /// 部門に所属する社員を取得（JOIN）
        /// </summary>
        public async Task<IEnumerable<dynamic>> FindEmployeesWithDepartmentAsync(string departmentCode)
        {
            const string sql = @"
                SELECT
                    e.社員コード,
                    e.社員名,
                    e.部門コード,
                    d.部門名
                FROM 社員マスタ e
                INNER JOIN 部門マスタ d ON e.部門コード = d.部門コード
                WHERE e.部門コード = @DepartmentCode
                ORDER BY e.社員コード";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync(sql, new { DepartmentCode = departmentCode });
        }
    }
}
