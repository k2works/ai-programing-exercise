using Xunit;
using FluentAssertions;
using Npgsql;
using Testcontainers.PostgreSql;

namespace AccountingSystem.Tests
{
    /// <summary>
    /// 勘定科目マスタのリファクタリングテスト
    /// </summary>
    public class AccountRefactoringTest : IAsyncLifetime
    {
        private readonly PostgreSqlContainer _postgres;
        private TestDatabase? _testDb;
        private NpgsqlConnection? _connection;

        public AccountRefactoringTest()
        {
            _postgres = new PostgreSqlBuilder()
                .WithImage("postgres:16-alpine")
                .WithDatabase("testdb")
                .WithUsername("testuser")
                .WithPassword("testpass")
                .Build();
        }

        public async Task InitializeAsync()
        {
            await _postgres.StartAsync();
            _testDb = new TestDatabase(_postgres);
            await _testDb.StartAsync();
            _connection = _testDb.GetConnection();
        }

        public async Task DisposeAsync()
        {
            if (_testDb != null)
            {
                await _testDb.StopAsync();
            }
            await _postgres.DisposeAsync();
        }

        private async Task CleanupAsync()
        {
            await _testDb!.CleanupAsync();
        }

        [Fact(DisplayName = "BSPL区分を設定できる")]
        public async Task TestBsplDistinction()
        {
            await CleanupAsync();

            // マイグレーション後、このテストが通るようになる
            var sql = @"
                INSERT INTO ""勘定科目マスタ""
                (""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""残高"")
                VALUES (@code, @name, @type::account_type, @bspl, @balance)
                RETURNING ""勘定科目コード"", ""BSPL区分""
            ";

            await using var cmd = new NpgsqlCommand(sql, _connection);
            cmd.Parameters.AddWithValue("code", "1000");
            cmd.Parameters.AddWithValue("name", "現金");
            cmd.Parameters.AddWithValue("type", "資産");
            cmd.Parameters.AddWithValue("bspl", "B");  // 貸借対照表
            cmd.Parameters.AddWithValue("balance", 0m);

            await using var reader = await cmd.ExecuteReaderAsync();

            (await reader.ReadAsync()).Should().BeTrue();
            reader.GetString(reader.GetOrdinal("BSPL区分")).Should().Be("B");
        }

        [Fact(DisplayName = "取引要素区分を設定できる")]
        public async Task TestTransactionDistinction()
        {
            await CleanupAsync();

            var sql = @"
                INSERT INTO ""勘定科目マスタ""
                (""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""BSPL区分"", ""取引要素区分"", ""残高"")
                VALUES (@code, @name, @type::account_type, @bspl, @trans, @balance)
                RETURNING ""勘定科目コード"", ""取引要素区分""
            ";

            await using var cmd = new NpgsqlCommand(sql, _connection);
            cmd.Parameters.AddWithValue("code", "1000");
            cmd.Parameters.AddWithValue("name", "現金");
            cmd.Parameters.AddWithValue("type", "資産");
            cmd.Parameters.AddWithValue("bspl", "B");
            cmd.Parameters.AddWithValue("trans", "1");  // 資産
            cmd.Parameters.AddWithValue("balance", 0m);

            await using var reader = await cmd.ExecuteReaderAsync();

            (await reader.ReadAsync()).Should().BeTrue();
            reader.GetString(reader.GetOrdinal("取引要素区分")).Should().Be("1");
        }

        [Fact(DisplayName = "合計科目を設定できる")]
        public async Task TestSumAccount()
        {
            await CleanupAsync();

            var sql = @"
                INSERT INTO ""勘定科目マスタ""
                (""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""合計科目"", ""残高"")
                VALUES (@code, @name, @type::account_type, @sum, @balance)
                RETURNING ""勘定科目コード"", ""合計科目""
            ";

            await using var cmd = new NpgsqlCommand(sql, _connection);
            cmd.Parameters.AddWithValue("code", "1000");
            cmd.Parameters.AddWithValue("name", "流動資産");
            cmd.Parameters.AddWithValue("type", "資産");
            cmd.Parameters.AddWithValue("sum", true);  // 集計科目フラグ
            cmd.Parameters.AddWithValue("balance", 0m);

            await using var reader = await cmd.ExecuteReaderAsync();

            (await reader.ReadAsync()).Should().BeTrue();
            reader.GetBoolean(reader.GetOrdinal("合計科目")).Should().BeTrue();
        }
    }
}
