using Moq;
using MRS.Application.DTOs;
using MRS.Application.Ports;
using MRS.Application.Services;
using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;

namespace MRS.Application.Tests.Services;

public class ReservationServiceTests
{
    private readonly Mock<IReservationRepository> _mockRepository;
    private readonly Mock<IUserRepository> _mockUserRepository;
    private readonly ReservationService _service;

    public ReservationServiceTests()
    {
        _mockRepository = new Mock<IReservationRepository>();
        _mockUserRepository = new Mock<IUserRepository>();
        _service = new ReservationService(_mockRepository.Object, _mockUserRepository.Object);
    }

    [Fact]
    public async Task GetReservationByIdAsync_WithValidId_ReturnsReservation()
    {
        // Arrange
        var reservationId = "test-id";
        var reservation = CreateReservationWithRowVersion("room1", "user1", "Test Meeting", DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2), 0);
        reservation.SetReservationId(reservationId);
        
        _mockRepository.Setup(r => r.GetByIdAsync(reservationId))
                      .ReturnsAsync(reservation);

        // Act
        var result = await _service.GetReservationByIdAsync(reservationId);

        // Assert
        Assert.NotNull(result);
        Assert.Equal(reservationId, result.ReservationId);
        Assert.Equal("Test Meeting", result.Title);
    }

    [Fact]
    public async Task GetReservationByIdAsync_WithInvalidId_ReturnsNull()
    {
        // Arrange
        var reservationId = "invalid-id";
        _mockRepository.Setup(r => r.GetByIdAsync(reservationId))
                      .ReturnsAsync((Reservation?)null);

        // Act
        var result = await _service.GetReservationByIdAsync(reservationId);

        // Assert
        Assert.Null(result);
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData(" ")]
    public async Task GetReservationByIdAsync_WithInvalidReservationId_ThrowsArgumentException(string? reservationId)
    {
        // Act & Assert
        await Assert.ThrowsAsync<ArgumentException>(() => _service.GetReservationByIdAsync(reservationId!));
    }

    [Fact]
    public async Task GetAllReservationsAsync_ReturnsAllReservations()
    {
        // Arrange
        var reservations = new List<Reservation>
        {
            CreateReservationWithRowVersion("room1", "user1", "Meeting 1", DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2), 0),
            CreateReservationWithRowVersion("room2", "user2", "Meeting 2", DateTime.UtcNow.AddHours(3), DateTime.UtcNow.AddHours(4), 0)
        };
        
        _mockRepository.Setup(r => r.GetAllAsync())
                      .ReturnsAsync(reservations.AsReadOnly());

        // Act
        var result = await _service.GetAllReservationsAsync();

        // Assert
        Assert.Equal(2, result.Count);
        Assert.Equal("Meeting 1", result[0].Title);
        Assert.Equal("Meeting 2", result[1].Title);
    }

    [Fact]
    public async Task GetReservationsByRoomIdAsync_WithValidRoomId_ReturnsReservations()
    {
        // Arrange
        var roomId = "room1";
        var reservations = new List<Reservation>
        {
            CreateReservationWithRowVersion(roomId, "user1", "Meeting 1", DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2), 0)
        };
        
        _mockRepository.Setup(r => r.GetByRoomIdAsync(roomId))
                      .ReturnsAsync(reservations.AsReadOnly());

        // Act
        var result = await _service.GetReservationsByRoomIdAsync(roomId);

        // Assert
        Assert.Single(result);
        Assert.Equal(roomId, result[0].RoomId);
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData(" ")]
    public async Task GetReservationsByRoomIdAsync_WithInvalidRoomId_ThrowsArgumentException(string? roomId)
    {
        // Act & Assert
        await Assert.ThrowsAsync<ArgumentException>(() => _service.GetReservationsByRoomIdAsync(roomId!));
    }

    [Fact]
    public async Task GetReservationsByUserIdAsync_WithValidUserId_ReturnsReservations()
    {
        // Arrange
        var userId = "user1";
        var reservations = new List<Reservation>
        {
            CreateReservationWithRowVersion("room1", userId, "Meeting 1", DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2), 0)
        };
        
        _mockRepository.Setup(r => r.GetByUserIdAsync(userId))
                      .ReturnsAsync(reservations.AsReadOnly());

        // Act
        var result = await _service.GetReservationsByUserIdAsync(userId);

        // Assert
        Assert.Single(result);
        Assert.Equal(userId, result[0].UserId);
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData(" ")]
    public async Task GetReservationsByUserIdAsync_WithInvalidUserId_ThrowsArgumentException(string? userId)
    {
        // Act & Assert
        await Assert.ThrowsAsync<ArgumentException>(() => _service.GetReservationsByUserIdAsync(userId!));
    }

    [Fact]
    public async Task GetReservationsByDateRangeAsync_WithValidRange_ReturnsReservations()
    {
        // Arrange
        var startDate = DateTime.UtcNow.Date;
        var endDate = startDate.AddDays(1);
        var reservations = new List<Reservation>
        {
            CreateReservationWithRowVersion("room1", "user1", "Meeting 1", startDate.AddHours(10), startDate.AddHours(11), 0)
        };
        
        _mockRepository.Setup(r => r.GetByDateRangeAsync(startDate, endDate))
                      .ReturnsAsync(reservations.AsReadOnly());

        // Act
        var result = await _service.GetReservationsByDateRangeAsync(startDate, endDate);

        // Assert
        Assert.Single(result);
    }

    [Fact]
    public async Task GetReservationsByDateRangeAsync_WithStartDateAfterEndDate_ThrowsArgumentException()
    {
        // Arrange
        var startDate = DateTime.UtcNow.AddDays(1);
        var endDate = DateTime.UtcNow;

        // Act & Assert
        await Assert.ThrowsAsync<ArgumentException>(() => _service.GetReservationsByDateRangeAsync(startDate, endDate));
    }

    [Fact]
    public async Task CreateReservationAsync_WithValidRequest_CreatesReservation()
    {
        // Arrange
        var request = new CreateReservationRequest(
            "room1",
            "user1",
            "Test Meeting",
            DateTime.UtcNow.AddHours(1),
            DateTime.UtcNow.AddHours(2),
            new List<string> { "participant1" }
        );

        var expectedId = "new-reservation-id";
        
        _mockRepository.Setup(r => r.HasConflictingReservationAsync(request.RoomId, request.StartTime, request.EndTime, null))
                      .ReturnsAsync(false);
        
        _mockRepository.Setup(r => r.CreateAsync(It.IsAny<Reservation>()))
                      .ReturnsAsync(expectedId);

        // Act
        var result = await _service.CreateReservationAsync(request);

        // Assert
        Assert.Equal(expectedId, result.ReservationId);
        Assert.Equal(request.RoomId, result.RoomId);
        Assert.Equal(request.UserId, result.UserId);
        Assert.Equal(request.Title, result.Title);
        Assert.Single(result.Participants);
        Assert.Equal("participant1", result.Participants[0]);
    }

    [Fact]
    public async Task CreateReservationAsync_WithNullRequest_ThrowsArgumentNullException()
    {
        // Act & Assert
        await Assert.ThrowsAsync<ArgumentNullException>(() => _service.CreateReservationAsync(null!));
    }

    [Fact]
    public async Task CreateReservationAsync_WithStartTimeAfterEndTime_ThrowsArgumentException()
    {
        // Arrange
        var request = new CreateReservationRequest(
            "room1",
            "user1",
            "Test Meeting",
            DateTime.UtcNow.AddHours(2),
            DateTime.UtcNow.AddHours(1)
        );

        // Act & Assert
        await Assert.ThrowsAsync<ArgumentException>(() => _service.CreateReservationAsync(request));
    }

    [Fact]
    public async Task CreateReservationAsync_WithPastTime_ThrowsArgumentException()
    {
        // Arrange
        var request = new CreateReservationRequest(
            "room1",
            "user1",
            "Test Meeting",
            DateTime.UtcNow.AddHours(-2),
            DateTime.UtcNow.AddHours(-1)
        );

        // Act & Assert
        await Assert.ThrowsAsync<ArgumentException>(() => _service.CreateReservationAsync(request));
    }

    [Fact]
    public async Task CreateReservationAsync_WithConflictingReservation_ThrowsInvalidOperationException()
    {
        // Arrange
        var request = new CreateReservationRequest(
            "room1",
            "user1",
            "Test Meeting",
            DateTime.UtcNow.AddHours(1),
            DateTime.UtcNow.AddHours(2)
        );

        _mockRepository.Setup(r => r.HasConflictingReservationAsync(request.RoomId, request.StartTime, request.EndTime, null))
                      .ReturnsAsync(true);

        // Act & Assert
        await Assert.ThrowsAsync<InvalidOperationException>(() => _service.CreateReservationAsync(request));
    }

    [Fact]
    public async Task UpdateReservationAsync_WithValidData_UpdatesReservation()
    {
        // Arrange
        var reservationId = "test-id";
        var existingReservation = CreateReservationWithRowVersion("room1", "user1", "Original Meeting", DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2), 1);
        existingReservation.SetReservationId(reservationId);

        var request = new UpdateReservationRequest(
            "Updated Meeting",
            DateTime.UtcNow.AddHours(2),
            DateTime.UtcNow.AddHours(3),
            new List<string> { "new-participant" }
        );

        _mockRepository.Setup(r => r.GetByIdAsync(reservationId))
                      .ReturnsAsync(existingReservation);
        
        _mockRepository.Setup(r => r.HasConflictingReservationAsync(existingReservation.RoomId, request.StartTime, request.EndTime, reservationId))
                      .ReturnsAsync(false);
        
        _mockRepository.Setup(r => r.UpdateAsync(It.IsAny<Reservation>()))
                      .ReturnsAsync(true);

        // Act
        var result = await _service.UpdateReservationAsync(reservationId, request, 1);

        // Assert
        Assert.Equal(reservationId, result.ReservationId);
        Assert.Equal("Updated Meeting", result.Title);
        Assert.Single(result.Participants);
        Assert.Equal("new-participant", result.Participants[0]);
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData(" ")]
    public async Task UpdateReservationAsync_WithInvalidReservationId_ThrowsArgumentException(string? reservationId)
    {
        // Arrange
        var request = new UpdateReservationRequest("Title", DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));

        // Act & Assert
        await Assert.ThrowsAsync<ArgumentException>(() => _service.UpdateReservationAsync(reservationId!, request, 0));
    }

    [Fact]
    public async Task UpdateReservationAsync_WithNullRequest_ThrowsArgumentNullException()
    {
        // Act & Assert
        await Assert.ThrowsAsync<ArgumentNullException>(() => _service.UpdateReservationAsync("test-id", null!, 0));
    }

    [Fact]
    public async Task UpdateReservationAsync_WithNonExistentReservation_ThrowsInvalidOperationException()
    {
        // Arrange
        var reservationId = "non-existent-id";
        var request = new UpdateReservationRequest("Title", DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));

        _mockRepository.Setup(r => r.GetByIdAsync(reservationId))
                      .ReturnsAsync((Reservation?)null);

        // Act & Assert
        await Assert.ThrowsAsync<InvalidOperationException>(() => _service.UpdateReservationAsync(reservationId, request, 0));
    }

    [Fact]
    public async Task UpdateReservationAsync_WithMismatchedRowVersion_ThrowsInvalidOperationException()
    {
        // Arrange
        var reservationId = "test-id";
        var existingReservation = CreateReservationWithRowVersion("room1", "user1", "Original Meeting", DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2), 2);
        existingReservation.SetReservationId(reservationId);

        var request = new UpdateReservationRequest("Updated Meeting", DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));

        _mockRepository.Setup(r => r.GetByIdAsync(reservationId))
                      .ReturnsAsync(existingReservation);

        // Act & Assert
        await Assert.ThrowsAsync<InvalidOperationException>(() => _service.UpdateReservationAsync(reservationId, request, 1));
    }

    [Fact]
    public async Task UpdateReservationAsync_WithStartTimeAfterEndTime_ThrowsArgumentException()
    {
        // Arrange
        var reservationId = "test-id";
        var existingReservation = CreateReservationWithRowVersion("room1", "user1", "Original Meeting", DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2), 1);
        existingReservation.SetReservationId(reservationId);

        var request = new UpdateReservationRequest(
            "Updated Meeting",
            DateTime.UtcNow.AddHours(3),
            DateTime.UtcNow.AddHours(2)
        );

        _mockRepository.Setup(r => r.GetByIdAsync(reservationId))
                      .ReturnsAsync(existingReservation);

        // Act & Assert
        await Assert.ThrowsAsync<ArgumentException>(() => _service.UpdateReservationAsync(reservationId, request, 1));
    }

    [Fact]
    public async Task UpdateReservationAsync_WithConflictingReservation_ThrowsInvalidOperationException()
    {
        // Arrange
        var reservationId = "test-id";
        var existingReservation = CreateReservationWithRowVersion("room1", "user1", "Original Meeting", DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2), 1);
        existingReservation.SetReservationId(reservationId);

        var request = new UpdateReservationRequest("Updated Meeting", DateTime.UtcNow.AddHours(2), DateTime.UtcNow.AddHours(3));

        _mockRepository.Setup(r => r.GetByIdAsync(reservationId))
                      .ReturnsAsync(existingReservation);
        
        _mockRepository.Setup(r => r.HasConflictingReservationAsync(existingReservation.RoomId, request.StartTime, request.EndTime, reservationId))
                      .ReturnsAsync(true);

        // Act & Assert
        await Assert.ThrowsAsync<InvalidOperationException>(() => _service.UpdateReservationAsync(reservationId, request, 1));
    }

    [Fact]
    public async Task UpdateReservationAsync_WhenUpdateFails_ThrowsInvalidOperationException()
    {
        // Arrange
        var reservationId = "test-id";
        var existingReservation = CreateReservationWithRowVersion("room1", "user1", "Original Meeting", DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2), 1);
        existingReservation.SetReservationId(reservationId);

        var request = new UpdateReservationRequest("Updated Meeting", DateTime.UtcNow.AddHours(2), DateTime.UtcNow.AddHours(3));

        _mockRepository.Setup(r => r.GetByIdAsync(reservationId))
                      .ReturnsAsync(existingReservation);
        
        _mockRepository.Setup(r => r.HasConflictingReservationAsync(existingReservation.RoomId, request.StartTime, request.EndTime, reservationId))
                      .ReturnsAsync(false);
        
        _mockRepository.Setup(r => r.UpdateAsync(It.IsAny<Reservation>()))
                      .ReturnsAsync(false);

        // Act & Assert
        await Assert.ThrowsAsync<InvalidOperationException>(() => _service.UpdateReservationAsync(reservationId, request, 1));
    }

    [Fact]
    public async Task CancelReservationAsync_WithValidId_ReturnsTrue()
    {
        // Arrange
        var reservationId = "test-id";
        
        _mockRepository.Setup(r => r.DeleteAsync(reservationId))
                      .ReturnsAsync(true);

        // Act
        var result = await _service.CancelReservationAsync(reservationId);

        // Assert
        Assert.True(result);
    }

    [Fact]
    public async Task CancelReservationAsync_WithNonExistentId_ReturnsFalse()
    {
        // Arrange
        var reservationId = "non-existent-id";
        
        _mockRepository.Setup(r => r.DeleteAsync(reservationId))
                      .ReturnsAsync(false);

        // Act
        var result = await _service.CancelReservationAsync(reservationId);

        // Assert
        Assert.False(result);
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData(" ")]
    public async Task CancelReservationAsync_WithInvalidReservationId_ThrowsArgumentException(string? reservationId)
    {
        // Act & Assert
        await Assert.ThrowsAsync<ArgumentException>(() => _service.CancelReservationAsync(reservationId!));
    }

    private static Reservation CreateReservationWithRowVersion(string roomId, string userId, string title, DateTime startTime, DateTime endTime, int rowVersion)
    {
        var timeSlot = new TimeSlot(startTime, endTime);
        var reservation = Reservation.Restore(
            string.Empty,
            roomId,
            userId,
            title,
            timeSlot,
            new List<string>(),
            "confirmed",
            rowVersion,
            DateTime.UtcNow,
            DateTime.UtcNow
        );
        return reservation;
    }
}