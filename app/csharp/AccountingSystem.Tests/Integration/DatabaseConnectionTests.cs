using Dapper;
using FluentAssertions;
using Npgsql;
using Xunit;

namespace AccountingSystem.Tests.Integration
{
    /// <summary>
    /// データベース接続テスト
    /// Testcontainersが正しく動作し、データベースに接続できることを確認します
    /// </summary>
    public class DatabaseConnectionTests : DatabaseTestBase
    {
        [Fact]
        public async Task データベースに接続できる()
        {
            // Arrange & Act
            await using var connection = new NpgsqlConnection(ConnectionString);
            await connection.OpenAsync();
            var result = await connection.QuerySingleAsync<int>("SELECT 1");

            // Assert
            result.Should().Be(1);
        }

        [Fact]
        public async Task FluentMigratorマイグレーションが実行されている()
        {
            // Arrange & Act
            await using var connection = new NpgsqlConnection(ConnectionString);
            await connection.OpenAsync();

            var count = await connection.QuerySingleAsync<long>(
                "SELECT COUNT(*) FROM information_schema.schemata WHERE schema_name = 'public'");

            // Assert
            count.Should().BeGreaterThan(0);
        }
    }
}
