using System.Data;
using Microsoft.Data.Sqlite;
using Xunit;
using MRS.Infrastructure.Data;
using MRS.Infrastructure.Repositories;
using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;

namespace MRS.Infrastructure.Tests.Repositories;

/// <summary>
/// UserRepositoryのテスト（Dapper版）
/// </summary>
public class UserRepositoryTests : IDisposable
{
    private readonly IDbConnection _connection;
    private readonly IDbConnectionFactory _connectionFactory;
    private readonly UserRepository _userRepository;

    public UserRepositoryTests()
    {
        // SQLiteインメモリデータベースを使用
        _connection = new SqliteConnection("Data Source=:memory:");
        _connection.Open();
        
        _connectionFactory = new TestConnectionFactory(_connection);
        _userRepository = new UserRepository(_connectionFactory);
        
        // テーブル作成
        InitializeDatabase();
    }

    private void InitializeDatabase()
    {
        const string createTableSql = @"
            CREATE TABLE IF NOT EXISTS Users (
                UserId TEXT NOT NULL PRIMARY KEY,
                Name TEXT NOT NULL UNIQUE,
                HashedPassword TEXT NOT NULL,
                Role TEXT NOT NULL,
                IsActive INTEGER NOT NULL DEFAULT 1,
                CreatedAt TEXT NOT NULL,
                UpdatedAt TEXT NOT NULL
            )";

        using var command = _connection.CreateCommand();
        command.CommandText = createTableSql;
        command.ExecuteNonQuery();
    }

    private void EnsureTableExists()
    {
        // 毎回テーブル作成を実行（IF NOT EXISTSで安全）
        InitializeDatabase();
        
        // テーブルのデータをクリーンアップ（テスト間の独立性を保証）
        ClearDatabase();
        
        // テーブルが実際に存在するかデバッグ確認
        const string checkTableSql = "SELECT name FROM sqlite_master WHERE type='table' AND name='Users'";
        using var command = _connection.CreateCommand();
        command.CommandText = checkTableSql;
        var result = command.ExecuteScalar();
        
        if (result == null)
        {
            throw new InvalidOperationException("Usersテーブルの作成に失敗しました");
        }
    }
    
    private void ClearDatabase()
    {
        const string deleteSql = "DELETE FROM Users";
        using var command = _connection.CreateCommand();
        command.CommandText = deleteSql;
        command.ExecuteNonQuery();
    }

    [Fact]
    public async Task GetByIdAsync_ExistingUser_ShouldReturnUser()
    {
        // Arrange
        EnsureTableExists(); // テーブルの存在を確認
        var userId = new UserId("user001");
        var userName = new Name("山田太郎");
        var password = new Password("Password123!");
        
        var user = new User(userId, userName, password);
        await _userRepository.AddAsync(user);

        // Act
        var result = await _userRepository.GetByIdAsync(userId);

        // Assert
        Assert.NotNull(result);
        Assert.Equal(userId, result.UserId);
        Assert.Equal(userName, result.Name);
        Assert.True(result.IsActive);
    }

    [Fact]
    public async Task GetByIdAsync_NonExistingUser_ShouldReturnNull()
    {
        // Arrange
        EnsureTableExists();
        var userId = new UserId("nonexistent");

        // Act
        var result = await _userRepository.GetByIdAsync(userId);

        // Assert
        Assert.Null(result);
    }

    [Fact]
    public async Task AddAsync_ValidUser_ShouldAddToDatabase()
    {
        // Arrange
        EnsureTableExists();
        var userId = new UserId("user002");
        var userName = new Name("佐藤花子");
        var password = new Password("Password456!");
        
        var user = new User(userId, userName, password);

        // Act
        await _userRepository.AddAsync(user);

        // Assert - 追加したユーザーを取得して確認
        var savedUser = await _userRepository.GetByIdAsync(userId);
        Assert.NotNull(savedUser);
        Assert.Equal(userId, savedUser.UserId);
        Assert.Equal(userName, savedUser.Name);
    }

    [Fact]
    public async Task UpdateAsync_ExistingUser_ShouldUpdateDatabase()
    {
        // Arrange
        EnsureTableExists();
        var userId = new UserId("user003");
        var userName = new Name("田中一郎");
        var password = new Password("Password789!");
        
        var user = new User(userId, userName, password);
        await _userRepository.AddAsync(user);

        // 名前を変更
        var newUserName = new Name("田中二郎");
        user.ChangeName(newUserName);

        // Act
        await _userRepository.UpdateAsync(user);

        // Assert
        var updatedUser = await _userRepository.GetByIdAsync(userId);
        Assert.NotNull(updatedUser);
        Assert.Equal(newUserName, updatedUser.Name);
    }

    [Fact]
    public async Task DeleteAsync_ExistingUser_ShouldRemoveFromDatabase()
    {
        // Arrange
        EnsureTableExists();
        var userId = new UserId("user004");
        var userName = new Name("鈴木三郎");
        var password = new Password("Password321!");
        
        var user = new User(userId, userName, password);
        await _userRepository.AddAsync(user);

        // Act
        await _userRepository.DeleteAsync(userId);

        // Assert
        var deletedUser = await _userRepository.GetByIdAsync(userId);
        Assert.Null(deletedUser);
    }

    [Fact]
    public async Task GetByNameAsync_ExistingName_ShouldReturnUser()
    {
        // Arrange
        EnsureTableExists();
        var userId = new UserId("user005");
        var userName = new Name("高橋四郎");
        var password = new Password("Password654!");
        
        var user = new User(userId, userName, password);
        await _userRepository.AddAsync(user);

        // Act
        var result = await _userRepository.GetByNameAsync(userName);

        // Assert
        Assert.NotNull(result);
        Assert.Equal(userName, result.Name);
        Assert.Equal(userId, result.UserId);
    }

    [Fact]
    public async Task GetByNameAsync_NonExistingName_ShouldReturnNull()
    {
        // Arrange
        EnsureTableExists();
        var userName = new Name("存在しないユーザー");

        // Act
        var result = await _userRepository.GetByNameAsync(userName);

        // Assert
        Assert.Null(result);
    }

    [Fact]
    public async Task GetAllAsync_MultipleUsers_ShouldReturnAllUsers()
    {
        // Arrange
        EnsureTableExists();
        var users = new List<User>
        {
            new(new UserId("user006"), new Name("ユーザー1"), new Password("Password1!")),
            new(new UserId("user007"), new Name("ユーザー2"), new Password("Password2!")),
            new(new UserId("user008"), new Name("ユーザー3"), new Password("Password3!"))
        };

        foreach (var user in users)
        {
            await _userRepository.AddAsync(user);
        }

        // Act
        var result = await _userRepository.GetAllAsync();

        // Assert
        var resultList = result.ToList();
        Assert.Equal(3, resultList.Count);
        Assert.Contains(resultList, u => u.UserId.Value == "user006");
        Assert.Contains(resultList, u => u.UserId.Value == "user007");
        Assert.Contains(resultList, u => u.UserId.Value == "user008");
    }

    public void Dispose()
    {
        _connection.Dispose();
    }

    /// <summary>
    /// テスト用の接続ファクトリー
    /// </summary>
    private class TestConnectionFactory : IDbConnectionFactory
    {
        private readonly IDbConnection _connection;

        public TestConnectionFactory(IDbConnection connection)
        {
            _connection = connection;
        }

        public IDbConnection CreateConnection()
        {
            // 共有接続をDispose()されないようにラップ
            return new NonDisposableConnectionWrapper(_connection);
        }
    }
    
    /// <summary>
    /// Dispose()を無効化する接続ラッパー
    /// </summary>
    private class NonDisposableConnectionWrapper : IDbConnection
    {
        private readonly IDbConnection _connection;

        public NonDisposableConnectionWrapper(IDbConnection connection)
        {
            _connection = connection;
        }

        public string ConnectionString 
        { 
            get => _connection.ConnectionString; 
            set => _connection.ConnectionString = value; 
        }

        public int ConnectionTimeout => _connection.ConnectionTimeout;
        public string Database => _connection.Database;
        public ConnectionState State => _connection.State;

        public IDbTransaction BeginTransaction() => _connection.BeginTransaction();
        public IDbTransaction BeginTransaction(IsolationLevel il) => _connection.BeginTransaction(il);
        public void ChangeDatabase(string databaseName) => _connection.ChangeDatabase(databaseName);
        public void Close() => _connection.Close();
        public IDbCommand CreateCommand() => _connection.CreateCommand();
        public void Open() => _connection.Open();

        // Dispose()を無効化
        public void Dispose()
        {
            // 何もしない - 実際の接続は閉じない
        }
    }
}
