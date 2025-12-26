using Dapper;
using FluentAssertions;
using ProductionManagement.Tests.TestSetup;

namespace ProductionManagement.Tests;

/// <summary>
/// データベース接続テスト
/// Testcontainers が正しく動作し、データベースに接続できることを確認します
/// </summary>
[Collection("Database")]
public class DatabaseConnectionTests
{
    private readonly PostgresFixture _fixture;

    public DatabaseConnectionTests(PostgresFixture fixture)
    {
        _fixture = fixture;
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task PostgreSQLに接続できる()
    {
        // Arrange & Act
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();
        var result = await connection.ExecuteScalarAsync<int>("SELECT 1");

        // Assert
        result.Should().Be(1);
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task FluentMigratorマイグレーションが実行されている()
    {
        // Arrange & Act
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();

        var count = await connection.QuerySingleAsync<long>(
            "SELECT COUNT(*) FROM information_schema.schemata WHERE schema_name = 'public'");

        // Assert
        count.Should().BeGreaterThan(0);
    }

    [Fact]
    [Trait("Category", "Integration")]
    public async Task 品目区分ENUMが作成されている()
    {
        // Arrange
        await using var connection = _fixture.CreateConnection();
        await connection.OpenAsync();

        // Act
        var result = await connection.QueryFirstAsync<string>(
            "SELECT unnest(enum_range(NULL::品目区分))::text LIMIT 1");

        // Assert
        result.Should().Be("製品");
    }
}
