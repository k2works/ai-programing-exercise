using System.Data;
using Dapper;
using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;
using MRS.Infrastructure.Data;
using MRS.Infrastructure.Repositories;

namespace MRS.Infrastructure.Tests.Repositories;

public class ReservationRepositoryTests : IDisposable
{
    private readonly TestDbConnectionFactory _connectionFactory;
    private readonly TestReservationRepository _repository;

    public ReservationRepositoryTests()
    {
        _connectionFactory = new TestDbConnectionFactory();
        _repository = new TestReservationRepository(_connectionFactory);
    }

    public void Dispose()
    {
        _connectionFactory.Dispose();
    }

    [Fact]
    public async Task CreateAsync_WithValidReservation_ReturnsReservationId()
    {
        // Arrange
        var timeSlot = new TimeSlot(DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));
        var reservation = Reservation.Create("room1", "user1", "Test Meeting", timeSlot, new List<string> { "participant1" });

        // Act
        var reservationId = await _repository.CreateAsync(reservation);

        // Assert
        Assert.NotNull(reservationId);
        Assert.NotEmpty(reservationId);
    }

    [Fact]
    public async Task GetByIdAsync_WithExistingId_ReturnsReservation()
    {
        // Arrange
        var timeSlot = new TimeSlot(DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));
        var reservation = Reservation.Create("room1", "user1", "Test Meeting", timeSlot, new List<string> { "participant1" });
        var reservationId = await _repository.CreateAsync(reservation);

        // Act
        var result = await _repository.GetByIdAsync(reservationId);

        // Assert
        Assert.NotNull(result);
        Assert.Equal(reservationId, result.ReservationId);
        Assert.Equal("room1", result.RoomId);
        Assert.Equal("user1", result.UserId);
        Assert.Equal("Test Meeting", result.Title);
        Assert.Single(result.Participants);
        Assert.Equal("participant1", result.Participants[0]);
    }

    [Fact]
    public async Task GetByIdAsync_WithNonExistentId_ReturnsNull()
    {
        // Act
        var result = await _repository.GetByIdAsync("non-existent-id");

        // Assert
        Assert.Null(result);
    }

    [Fact]
    public async Task GetAllAsync_WithMultipleReservations_ReturnsAllOrderedByStartTime()
    {
        // Arrange
        var timeSlot1 = new TimeSlot(DateTime.UtcNow.AddHours(2), DateTime.UtcNow.AddHours(3));
        var timeSlot2 = new TimeSlot(DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));
        
        var reservation1 = Reservation.Create("room1", "user1", "Meeting 1", timeSlot1, new List<string>());
        var reservation2 = Reservation.Create("room2", "user2", "Meeting 2", timeSlot2, new List<string>());

        await _repository.CreateAsync(reservation1);
        await _repository.CreateAsync(reservation2);

        // Act
        var result = await _repository.GetAllAsync();

        // Assert
        Assert.Equal(2, result.Count);
        Assert.Equal("Meeting 2", result[0].Title); // Earlier start time should come first
        Assert.Equal("Meeting 1", result[1].Title);
    }

    [Fact]
    public async Task GetByRoomIdAsync_WithValidRoomId_ReturnsReservationsForRoom()
    {
        // Arrange
        var roomId = "room1";
        var timeSlot1 = new TimeSlot(DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));
        var timeSlot2 = new TimeSlot(DateTime.UtcNow.AddHours(3), DateTime.UtcNow.AddHours(4));
        
        var reservation1 = Reservation.Create(roomId, "user1", "Meeting 1", timeSlot1, new List<string>());
        var reservation2 = Reservation.Create("room2", "user2", "Meeting 2", timeSlot2, new List<string>());

        await _repository.CreateAsync(reservation1);
        await _repository.CreateAsync(reservation2);

        // Act
        var result = await _repository.GetByRoomIdAsync(roomId);

        // Assert
        Assert.Single(result);
        Assert.Equal(roomId, result[0].RoomId);
        Assert.Equal("Meeting 1", result[0].Title);
    }

    [Fact]
    public async Task GetByUserIdAsync_WithValidUserId_ReturnsReservationsForUser()
    {
        // Arrange
        var userId = "user1";
        var timeSlot1 = new TimeSlot(DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));
        var timeSlot2 = new TimeSlot(DateTime.UtcNow.AddHours(3), DateTime.UtcNow.AddHours(4));
        
        var reservation1 = Reservation.Create("room1", userId, "Meeting 1", timeSlot1, new List<string>());
        var reservation2 = Reservation.Create("room2", "user2", "Meeting 2", timeSlot2, new List<string>());

        await _repository.CreateAsync(reservation1);
        await _repository.CreateAsync(reservation2);

        // Act
        var result = await _repository.GetByUserIdAsync(userId);

        // Assert
        Assert.Single(result);
        Assert.Equal(userId, result[0].UserId);
        Assert.Equal("Meeting 1", result[0].Title);
    }

    [Fact]
    public async Task GetByDateRangeAsync_WithValidRange_ReturnsReservationsInRange()
    {
        // Arrange
        var baseDate = DateTime.UtcNow.Date;
        var startDate = baseDate.AddDays(1);
        var endDate = baseDate.AddDays(2);

        var timeSlotInRange = new TimeSlot(startDate.AddHours(10), startDate.AddHours(11));
        var timeSlotOutOfRange = new TimeSlot(baseDate.AddHours(10), baseDate.AddHours(11));
        
        var reservationInRange = Reservation.Create("room1", "user1", "Meeting In Range", timeSlotInRange, new List<string>());
        var reservationOutOfRange = Reservation.Create("room2", "user2", "Meeting Out Of Range", timeSlotOutOfRange, new List<string>());

        await _repository.CreateAsync(reservationInRange);
        await _repository.CreateAsync(reservationOutOfRange);

        // Act
        var result = await _repository.GetByDateRangeAsync(startDate, endDate);

        // Assert
        Assert.Single(result);
        Assert.Equal("Meeting In Range", result[0].Title);
    }

    [Fact]
    public async Task HasConflictingReservationAsync_WithConflictingTime_ReturnsTrue()
    {
        // Arrange
        var roomId = "room1";
        var timeSlot = new TimeSlot(DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));
        var reservation = Reservation.Create(roomId, "user1", "Existing Meeting", timeSlot, new List<string>());
        
        var createdId = await _repository.CreateAsync(reservation);
        
        // Debug: 実際に保存されたデータを確認
        var savedReservation = await _repository.GetByIdAsync(createdId);
        Assert.NotNull(savedReservation);

        var conflictingStartTime = DateTime.UtcNow.AddHours(1).AddMinutes(30);
        var conflictingEndTime = DateTime.UtcNow.AddHours(2).AddMinutes(30);

        // Act
        var result = await _repository.HasConflictingReservationAsync(roomId, conflictingStartTime, conflictingEndTime);

        // Assert
        Assert.True(result);
    }

    [Fact]
    public async Task HasConflictingReservationAsync_WithNonConflictingTime_ReturnsFalse()
    {
        // Arrange
        var roomId = "room1";
        var timeSlot = new TimeSlot(DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));
        var reservation = Reservation.Create(roomId, "user1", "Existing Meeting", timeSlot, new List<string>());
        
        await _repository.CreateAsync(reservation);

        var nonConflictingStartTime = DateTime.UtcNow.AddHours(3);
        var nonConflictingEndTime = DateTime.UtcNow.AddHours(4);

        // Act
        var result = await _repository.HasConflictingReservationAsync(roomId, nonConflictingStartTime, nonConflictingEndTime);

        // Assert
        Assert.False(result);
    }

    [Fact]
    public async Task HasConflictingReservationAsync_ExcludingExistingReservation_ReturnsFalse()
    {
        // Arrange
        var roomId = "room1";
        var timeSlot = new TimeSlot(DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));
        var reservation = Reservation.Create(roomId, "user1", "Existing Meeting", timeSlot, new List<string>());
        
        var reservationId = await _repository.CreateAsync(reservation);

        // Act - Check for conflict excluding the same reservation
        var result = await _repository.HasConflictingReservationAsync(roomId, timeSlot.StartTime, timeSlot.EndTime, reservationId);

        // Assert
        Assert.False(result);
    }

    [Fact]
    public async Task UpdateAsync_WithValidData_UpdatesReservationAndIncrementsRowVersion()
    {
        // Arrange
        var timeSlot = new TimeSlot(DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));
        var reservation = Reservation.Create("room1", "user1", "Original Title", timeSlot, new List<string>());
        var reservationId = await _repository.CreateAsync(reservation);
        
        // データベースから最新の予約を取得
        var dbReservation = await _repository.GetByIdAsync(reservationId);
        Assert.NotNull(dbReservation);
        
        // Update 前のRowVersionを保存
        var originalRowVersion = dbReservation.RowVersion;

        var newTimeSlot = new TimeSlot(DateTime.UtcNow.AddHours(2), DateTime.UtcNow.AddHours(3));
        dbReservation.Update("Updated Title", newTimeSlot, new List<string> { "new-participant" });
        
        // UpdateAsync用にRowVersionを元に戻す（楽観的ロック用）
        dbReservation.UpdateRowVersion(originalRowVersion);

        // Act
        var result = await _repository.UpdateAsync(dbReservation);

        // Assert
        Assert.True(result);

        var updatedReservation = await _repository.GetByIdAsync(reservationId);
        Assert.NotNull(updatedReservation);
        Assert.Equal("Updated Title", updatedReservation.Title);
        Assert.Single(updatedReservation.Participants);
        Assert.Equal("new-participant", updatedReservation.Participants[0]);
        Assert.Equal(1, updatedReservation.RowVersion); // Should increment from 0 to 1
    }

    [Fact]
    public async Task UpdateAsync_WithStaleRowVersion_ReturnsFalse()
    {
        // Arrange
        var timeSlot = new TimeSlot(DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));
        var reservation = Reservation.Create("room1", "user1", "Original Title", timeSlot, new List<string>());
        var reservationId = await _repository.CreateAsync(reservation);
        reservation.SetReservationId(reservationId);

        // Simulate concurrent update by manually updating row version
        reservation.UpdateRowVersion(999); // Set stale row version

        var newTimeSlot = new TimeSlot(DateTime.UtcNow.AddHours(2), DateTime.UtcNow.AddHours(3));
        reservation.Update("Updated Title", newTimeSlot, new List<string>());

        // Act
        var result = await _repository.UpdateAsync(reservation);

        // Assert
        Assert.False(result);
    }

    [Fact]
    public async Task DeleteAsync_WithExistingId_DeletesReservationAndReturnsTrue()
    {
        // Arrange
        var timeSlot = new TimeSlot(DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));
        var reservation = Reservation.Create("room1", "user1", "Test Meeting", timeSlot, new List<string>());
        var reservationId = await _repository.CreateAsync(reservation);

        // Act
        var result = await _repository.DeleteAsync(reservationId);

        // Assert
        Assert.True(result);

        var deletedReservation = await _repository.GetByIdAsync(reservationId);
        Assert.Null(deletedReservation);
    }

    [Fact]
    public async Task DeleteAsync_WithNonExistentId_ReturnsFalse()
    {
        // Act
        var result = await _repository.DeleteAsync("non-existent-id");

        // Assert
        Assert.False(result);
    }
}

internal class TestReservationRepository : ReservationRepository
{
    private readonly IDbConnectionFactory _testConnectionFactory;

    public TestReservationRepository(IDbConnectionFactory connectionFactory) 
        : base(connectionFactory)
    {
        _testConnectionFactory = connectionFactory;
        InitializeTestDatabaseAsync().GetAwaiter().GetResult();
    }

    private async Task InitializeTestDatabaseAsync()
    {
        using var connection = _testConnectionFactory.CreateConnection();
        
        const string createTableSql = @"
            CREATE TABLE IF NOT EXISTS Reservations (
                ReservationId VARCHAR(50) NOT NULL PRIMARY KEY,
                RoomId VARCHAR(50) NOT NULL,
                UserId VARCHAR(50) NOT NULL,
                Title VARCHAR(200) NOT NULL,
                StartTime TIMESTAMP NOT NULL,
                EndTime TIMESTAMP NOT NULL,
                Participants TEXT,
                Status VARCHAR(20) DEFAULT 'confirmed',
                RowVersion INTEGER DEFAULT 0,
                CreatedAt TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
                UpdatedAt TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
            );";
        
        await connection.ExecuteAsync(createTableSql);
    }
}

internal class TestDbConnectionFactory : IDbConnectionFactory, IDisposable
{
    private readonly object Lock = new();
    private IDbConnection? SharedConnection;

    public TestDbConnectionFactory()
    {
        lock (Lock)
        {
            SharedConnection = new Microsoft.Data.Sqlite.SqliteConnection("Data Source=:memory:");
            SharedConnection.Open();
            
            // Enable foreign key support and other pragmas
            using var command = SharedConnection.CreateCommand();
            command.CommandText = "PRAGMA foreign_keys = ON;";
            command.ExecuteNonQuery();
        }
    }

    public IDbConnection CreateConnection()
    {
        lock (Lock)
        {
            if (SharedConnection == null)
                throw new ObjectDisposedException(nameof(TestDbConnectionFactory));
            return new NonDisposableConnectionWrapper(SharedConnection);
        }
    }

    public void Dispose()
    {
        lock (Lock)
        {
            SharedConnection?.Dispose();
            SharedConnection = null;
        }
    }
}

internal sealed class NonDisposableConnectionWrapper : IDbConnection
{
    private readonly IDbConnection _connection;

    public NonDisposableConnectionWrapper(IDbConnection connection)
    {
        _connection = connection;
    }

    public void Dispose() { /* Intentionally do nothing */ }

    public IDbTransaction BeginTransaction() => _connection.BeginTransaction();
    public IDbTransaction BeginTransaction(IsolationLevel il) => _connection.BeginTransaction(il);
    public void ChangeDatabase(string databaseName) => _connection.ChangeDatabase(databaseName);
    public void Close() => _connection.Close();
    public IDbCommand CreateCommand() => _connection.CreateCommand();
    public void Open() => _connection.Open();
    public string ConnectionString
    {
        get => _connection.ConnectionString;
        set => _connection.ConnectionString = value;
    }
    public int ConnectionTimeout => _connection.ConnectionTimeout;
    public string Database => _connection.Database;
    public ConnectionState State => _connection.State;
}