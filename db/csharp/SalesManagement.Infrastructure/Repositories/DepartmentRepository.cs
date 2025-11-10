using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    /// <summary>
    /// 部門マスタのRepositoryクラス
    /// </summary>
    public class DepartmentRepository
    {
        private readonly string _connectionString;

        public DepartmentRepository(string connectionString)
        {
            _connectionString = connectionString;
        }

        /// <summary>
        /// 部門を登録
        /// </summary>
        public async Task InsertAsync(Department department)
        {
            const string sql = @"
                INSERT INTO 部門マスタ (
                    部門コード, 開始日, 終了日, 部門名, 組織階層, 部門パス, 最下層区分, 伝票入力可否,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @DepartmentCode, @StartDate, @EndDate, @DepartmentName,
                    @OrganizationLevel, @DepartmentPath, @LowestLevelFlag, @SlipInputFlag,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, department);
        }

        /// <summary>
        /// 部門を更新
        /// </summary>
        public async Task UpdateAsync(Department department)
        {
            const string sql = @"
                UPDATE 部門マスタ
                SET 部門名 = @DepartmentName,
                    組織階層 = @OrganizationLevel,
                    部門パス = @DepartmentPath,
                    最下層区分 = @LowestLevelFlag,
                    伝票入力可否 = @SlipInputFlag,
                    更新日時 = @UpdatedAt,
                    更新者名 = @UpdatedBy
                WHERE 部門コード = @DepartmentCode
                  AND 開始日 = @StartDate";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, department);
        }

        /// <summary>
        /// 部門を削除
        /// </summary>
        public async Task DeleteAsync(string departmentCode, DateTime startDate)
        {
            const string sql = @"
                DELETE FROM 部門マスタ
                WHERE 部門コード = @DepartmentCode
                  AND 開始日 = @StartDate";

            await using var connection = new NpgsqlConnection(_connectionString);
            await connection.ExecuteAsync(sql, new { DepartmentCode = departmentCode, StartDate = startDate });
        }

        /// <summary>
        /// 主キーで部門を検索
        /// </summary>
        public async Task<Department?> FindByIdAsync(string departmentCode, DateTime startDate)
        {
            const string sql = @"
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
                  AND 開始日 = @StartDate";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QuerySingleOrDefaultAsync<Department>(sql,
                new { DepartmentCode = departmentCode, StartDate = startDate });
        }

        /// <summary>
        /// すべての部門を取得
        /// </summary>
        public async Task<IEnumerable<Department>> FindAllAsync()
        {
            const string sql = @"
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
                ORDER BY 組織階層, 部門コード, 開始日";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Department>(sql);
        }

        /// <summary>
        /// 部門コードで検索（履歴含む）
        /// </summary>
        public async Task<IEnumerable<Department>> FindByDepartmentCodeAsync(string departmentCode)
        {
            const string sql = @"
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
                ORDER BY 開始日";

            await using var connection = new NpgsqlConnection(_connectionString);
            return await connection.QueryAsync<Department>(sql, new { DepartmentCode = departmentCode });
        }
    }
}
