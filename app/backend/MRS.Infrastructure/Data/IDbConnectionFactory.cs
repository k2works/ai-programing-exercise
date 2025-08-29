using System.Data;
using Npgsql;

namespace MRS.Infrastructure.Data;

/// <summary>
/// データベース接続ファクトリー
/// </summary>
public interface IDbConnectionFactory
{
    /// <summary>
    /// データベース接続を作成
    /// </summary>
    /// <returns>データベース接続</returns>
    IDbConnection CreateConnection();
}

/// <summary>
/// PostgreSQL接続ファクトリー
/// </summary>
public class PostgreSqlConnectionFactory : IDbConnectionFactory
{
    private readonly string _connectionString;

    public PostgreSqlConnectionFactory(string connectionString)
    {
        _connectionString = connectionString ?? throw new ArgumentNullException(nameof(connectionString));
    }

    /// <summary>
    /// PostgreSQL接続を作成
    /// </summary>
    public IDbConnection CreateConnection()
    {
        return new NpgsqlConnection(_connectionString);
    }
}

/// <summary>
/// インメモリデータベース接続ファクトリー（テスト用）
/// </summary>
public class InMemoryConnectionFactory : IDbConnectionFactory
{
    private readonly string _connectionString;

    public InMemoryConnectionFactory()
    {
        // SQLiteインメモリデータベースを使用
        _connectionString = "Data Source=:memory:";
    }

    public IDbConnection CreateConnection()
    {
        // テスト用にはSQLiteを使用（PostgreSQLとSQL互換性を保つ）
        var connection = new Microsoft.Data.Sqlite.SqliteConnection(_connectionString);
        connection.Open();
        return connection;
    }
}
