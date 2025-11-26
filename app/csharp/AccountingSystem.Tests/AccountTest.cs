using Xunit;
using FluentAssertions;
using Npgsql;
using Dapper;

namespace AccountingSystem.Tests
{
    /// <summary>
    /// 勘定科目マスタのテスト
    ///
    /// このテストでは、勘定科目マスタに対するCRUD操作を検証します。
    /// Testcontainersを使用して、実際のPostgreSQLコンテナでテストを実行します。
    /// </summary>
    public class AccountTest : DatabaseTestBase
    {
        private async Task CleanupAsync()
        {
            await using var connection = new NpgsqlConnection(ConnectionString);
            await connection.OpenAsync();
            await connection.ExecuteAsync(@"TRUNCATE TABLE ""勘定科目マスタ"" CASCADE");
        }

        [Fact(DisplayName = "勘定科目を登録できる")]
        public async Task TestCreateAccount()
        {
            // 各テスト前にデータをクリア
            await CleanupAsync();

            await using var connection = new NpgsqlConnection(ConnectionString);
            await connection.OpenAsync();

            // 1. テストデータを作成
            var sql = @"
                INSERT INTO ""勘定科目マスタ"" (""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""残高"")
                VALUES (@code, @name, @type::account_type, @balance)
                RETURNING ""勘定科目ID"", ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別""::text, ""残高""
            ";

            await using var cmd = new NpgsqlCommand(sql, connection);
            cmd.Parameters.AddWithValue("code", "1000");
            cmd.Parameters.AddWithValue("name", "現金");
            cmd.Parameters.AddWithValue("type", "資産");
            cmd.Parameters.AddWithValue("balance", 50000.00m);

            await using var reader = await cmd.ExecuteReaderAsync();

            // 2. 取得したデータが期待通りか検証
            (await reader.ReadAsync()).Should().BeTrue();
            reader.GetString(reader.GetOrdinal("勘定科目コード")).Should().Be("1000");
            reader.GetString(reader.GetOrdinal("勘定科目名")).Should().Be("現金");
            reader.GetString(reader.GetOrdinal("勘定科目種別")).Should().Be("資産");
            reader.GetDecimal(reader.GetOrdinal("残高")).Should().Be(50000.00m);
        }

        [Fact(DisplayName = "すべての勘定科目を取得できる")]
        public async Task TestFindAllAccounts()
        {
            await CleanupAsync();

            await using var connection = new NpgsqlConnection(ConnectionString);
            await connection.OpenAsync();

            // 1. 複数の勘定科目を登録
            await InsertAccountAsync(connection, "1000", "現金", "資産", 50000.00m);
            await InsertAccountAsync(connection, "2000", "買掛金", "負債", 30000.00m);
            await InsertAccountAsync(connection, "3000", "資本金", "純資産", 100000.00m);

            // 2. すべての勘定科目を取得
            var sql = @"
                SELECT ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""残高""
                FROM ""勘定科目マスタ""
                ORDER BY ""勘定科目コード""
            ";

            var codes = new List<string>();
            await using var cmd = new NpgsqlCommand(sql, connection);
            await using var reader = await cmd.ExecuteReaderAsync();

            while (await reader.ReadAsync())
            {
                codes.Add(reader.GetString(reader.GetOrdinal("勘定科目コード")));
            }

            // 3. 期待通りのデータが取得できるか検証
            codes.Should().HaveCount(3);
            codes.Should().ContainInOrder("1000", "2000", "3000");
        }

        [Fact(DisplayName = "勘定科目コードで検索できる")]
        public async Task TestFindAccountByCode()
        {
            await CleanupAsync();

            await using var connection = new NpgsqlConnection(ConnectionString);
            await connection.OpenAsync();

            // 1. テストデータを登録
            await InsertAccountAsync(connection, "1000", "現金", "資産", 50000.00m);

            // 2. コードで検索
            var sql = @"
                SELECT ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別""::text
                FROM ""勘定科目マスタ""
                WHERE ""勘定科目コード"" = @code
            ";

            await using var cmd = new NpgsqlCommand(sql, connection);
            cmd.Parameters.AddWithValue("code", "1000");
            await using var reader = await cmd.ExecuteReaderAsync();

            // 3. 正しいデータが取得できるか検証
            (await reader.ReadAsync()).Should().BeTrue();
            reader.GetString(reader.GetOrdinal("勘定科目名")).Should().Be("現金");
            reader.GetString(reader.GetOrdinal("勘定科目種別")).Should().Be("資産");
        }

        [Fact(DisplayName = "勘定科目を更新できる")]
        public async Task TestUpdateAccount()
        {
            await CleanupAsync();

            await using var connection = new NpgsqlConnection(ConnectionString);
            await connection.OpenAsync();

            // 1. データを登録
            int accountId = await InsertAccountAsync(connection, "1000", "現金", "資産", 50000.00m);

            // 2. データを更新
            var updateSql = @"
                UPDATE ""勘定科目マスタ""
                SET ""勘定科目名"" = @name, ""残高"" = @balance, ""更新日時"" = CURRENT_TIMESTAMP
                WHERE ""勘定科目ID"" = @id
            ";

            await using (var cmd = new NpgsqlCommand(updateSql, connection))
            {
                cmd.Parameters.AddWithValue("name", "現金及び預金");
                cmd.Parameters.AddWithValue("balance", 75000.00m);
                cmd.Parameters.AddWithValue("id", accountId);
                var updated = await cmd.ExecuteNonQueryAsync();
                updated.Should().Be(1);
            }

            // 3. 更新されたか検証
            var selectSql = @"
                SELECT ""勘定科目コード"", ""勘定科目名"", ""残高""
                FROM ""勘定科目マスタ""
                WHERE ""勘定科目ID"" = @id
            ";

            await using (var cmd = new NpgsqlCommand(selectSql, connection))
            {
                cmd.Parameters.AddWithValue("id", accountId);
                await using var reader = await cmd.ExecuteReaderAsync();

                (await reader.ReadAsync()).Should().BeTrue();
                reader.GetString(reader.GetOrdinal("勘定科目名")).Should().Be("現金及び預金");
                reader.GetDecimal(reader.GetOrdinal("残高")).Should().Be(75000.00m);
                reader.GetString(reader.GetOrdinal("勘定科目コード")).Should().Be("1000"); // 変更していない項目は保持される
            }
        }

        [Fact(DisplayName = "勘定科目を削除できる")]
        public async Task TestDeleteAccount()
        {
            await CleanupAsync();

            await using var connection = new NpgsqlConnection(ConnectionString);
            await connection.OpenAsync();

            // 1. データを登録
            int accountId = await InsertAccountAsync(connection, "1000", "現金", "資産", 50000.00m);

            // 2. データを削除
            var deleteSql = @"
                DELETE FROM ""勘定科目マスタ""
                WHERE ""勘定科目ID"" = @id
            ";

            await using (var cmd = new NpgsqlCommand(deleteSql, connection))
            {
                cmd.Parameters.AddWithValue("id", accountId);
                var deleted = await cmd.ExecuteNonQueryAsync();
                deleted.Should().Be(1);
            }

            // 3. データが削除されたか検証
            var selectSql = @"
                SELECT COUNT(*) as count
                FROM ""勘定科目マスタ""
                WHERE ""勘定科目ID"" = @id
            ";

            await using (var cmd = new NpgsqlCommand(selectSql, connection))
            {
                cmd.Parameters.AddWithValue("id", accountId);
                var count = (long)(await cmd.ExecuteScalarAsync())!;
                count.Should().Be(0);
            }
        }

        [Fact(DisplayName = "勘定科目種別でフィルタリングできる")]
        public async Task TestFilterAccountsByType()
        {
            await CleanupAsync();

            await using var connection = new NpgsqlConnection(ConnectionString);
            await connection.OpenAsync();

            // 1. 複数の勘定科目を登録
            await InsertAccountAsync(connection, "1000", "現金", "資産", 50000.00m);
            await InsertAccountAsync(connection, "2000", "買掛金", "負債", 30000.00m);
            await InsertAccountAsync(connection, "3000", "資本金", "純資産", 100000.00m);

            // 2. 資産勘定のみを取得
            var sql = @"
                SELECT ""勘定科目名""
                FROM ""勘定科目マスタ""
                WHERE ""勘定科目種別"" = @type::account_type
            ";

            var assetNames = new List<string>();
            await using (var cmd = new NpgsqlCommand(sql, connection))
            {
                cmd.Parameters.AddWithValue("type", "資産");
                await using var reader = await cmd.ExecuteReaderAsync();
                while (await reader.ReadAsync())
                {
                    assetNames.Add(reader.GetString(0));
                }
            }

            // 3. 正しくフィルタリングされるか検証
            assetNames.Should().HaveCount(1);
            assetNames.Should().ContainInOrder("現金");

            // 4. 負債勘定のみを取得
            var liabilityNames = new List<string>();
            await using (var cmd = new NpgsqlCommand(sql, connection))
            {
                cmd.Parameters.AddWithValue("type", "負債");
                await using var reader = await cmd.ExecuteReaderAsync();
                while (await reader.ReadAsync())
                {
                    liabilityNames.Add(reader.GetString(0));
                }
            }

            liabilityNames.Should().HaveCount(1);
            liabilityNames.Should().ContainInOrder("買掛金");
        }

        /// <summary>
        /// 勘定科目を登録するヘルパーメソッド
        /// </summary>
        private static async Task<int> InsertAccountAsync(NpgsqlConnection connection, string code, string name, string type, decimal balance)
        {
            var sql = @"
                INSERT INTO ""勘定科目マスタ"" (""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""残高"")
                VALUES (@code, @name, @type::account_type, @balance)
                RETURNING ""勘定科目ID""
            ";

            await using var cmd = new NpgsqlCommand(sql, connection);
            cmd.Parameters.AddWithValue("code", code);
            cmd.Parameters.AddWithValue("name", name);
            cmd.Parameters.AddWithValue("type", type);
            cmd.Parameters.AddWithValue("balance", balance);

            var result = await cmd.ExecuteScalarAsync();
            return Convert.ToInt32(result, System.Globalization.CultureInfo.InvariantCulture);
        }
    }
}
