using MRS.Domain.Entities;
using MRS.Domain.Services;
using MRS.Domain.ValueObjects;
using Xunit;

namespace MRS.Domain.Tests.Services;

public class ReservationDomainServiceTests
{
    [Fact]
    public void CanReserve_WithNoConflicts_ShouldReturnTrue()
    {
        // Arrange
        var service = new ReservationDomainService();
        var newReservation = CreateTestReservation("res-new", "2025-09-04T10:00:00Z", "2025-09-04T11:00:00Z");
        var existingReservations = new List<Reservation>
        {
            CreateTestReservation("res-1", "2025-09-04T08:00:00Z", "2025-09-04T09:00:00Z"),
            CreateTestReservation("res-2", "2025-09-04T14:00:00Z", "2025-09-04T15:00:00Z")
        };

        // Act - Red状態（コンパイルエラー）
        var result = service.CanReserve(newReservation, existingReservations);

        // Assert
        Assert.True(result);
    }

    [Fact]
    public void CanReserve_WithTimeConflict_ShouldReturnFalse()
    {
        // Arrange
        var service = new ReservationDomainService();
        var newReservation = CreateTestReservation("res-new", "2025-09-04T10:00:00Z", "2025-09-04T11:00:00Z");
        var existingReservations = new List<Reservation>
        {
            CreateTestReservation("res-1", "2025-09-04T09:30:00Z", "2025-09-04T10:30:00Z") // 重複あり
        };

        // Act
        var result = service.CanReserve(newReservation, existingReservations);

        // Assert
        Assert.False(result);
    }

    [Fact]
    public void CanReserve_WithDifferentRooms_ShouldReturnTrue()
    {
        // Arrange
        var service = new ReservationDomainService();
        var newReservation = CreateTestReservation("res-new", "2025-09-04T10:00:00Z", "2025-09-04T11:00:00Z", "room-001");
        var existingReservations = new List<Reservation>
        {
            CreateTestReservation("res-1", "2025-09-04T10:00:00Z", "2025-09-04T11:00:00Z", "room-002") // 別の部屋
        };

        // Act
        var result = service.CanReserve(newReservation, existingReservations);

        // Assert
        Assert.True(result);
    }

    [Fact]
    public void GetConflictingReservations_WithConflicts_ShouldReturnConflicts()
    {
        // Arrange
        var service = new ReservationDomainService();
        var newReservation = CreateTestReservation("res-new", "2025-09-04T10:00:00Z", "2025-09-04T12:00:00Z");
        var conflictingReservation = CreateTestReservation("res-conflict", "2025-09-04T11:00:00Z", "2025-09-04T13:00:00Z");
        var existingReservations = new List<Reservation>
        {
            CreateTestReservation("res-safe", "2025-09-04T08:00:00Z", "2025-09-04T09:00:00Z"),
            conflictingReservation
        };

        // Act
        var conflicts = service.GetConflictingReservations(newReservation, existingReservations);

        // Assert
        Assert.Single(conflicts);
        Assert.Equal(conflictingReservation.ReservationId, conflicts.First().ReservationId);
    }

    [Fact]
    public void GetConflictingReservations_WithNoConflicts_ShouldReturnEmpty()
    {
        // Arrange
        var service = new ReservationDomainService();
        var newReservation = CreateTestReservation("res-new", "2025-09-04T10:00:00Z", "2025-09-04T11:00:00Z");
        var existingReservations = new List<Reservation>
        {
            CreateTestReservation("res-1", "2025-09-04T08:00:00Z", "2025-09-04T09:00:00Z"),
            CreateTestReservation("res-2", "2025-09-04T14:00:00Z", "2025-09-04T15:00:00Z")
        };

        // Act
        var conflicts = service.GetConflictingReservations(newReservation, existingReservations);

        // Assert
        Assert.Empty(conflicts);
    }

    [Theory]
    [InlineData("2025-09-04T09:00:00Z", "2025-09-04T11:00:00Z")]  // 開始時刻重複
    [InlineData("2025-09-04T10:30:00Z", "2025-09-04T12:00:00Z")]  // 終了時刻重複
    [InlineData("2025-09-04T09:00:00Z", "2025-09-04T12:00:00Z")]  // 完全包含
    [InlineData("2025-09-04T10:15:00Z", "2025-09-04T10:45:00Z")]  // 内包
    public void CanReserve_WithVariousConflicts_ShouldReturnFalse(string startTime, string endTime)
    {
        // Arrange
        var service = new ReservationDomainService();
        var baseReservation = CreateTestReservation("res-base", "2025-09-04T10:00:00Z", "2025-09-04T11:00:00Z");
        var conflictingReservation = CreateTestReservation("res-conflict", startTime, endTime);
        var existingReservations = new List<Reservation> { baseReservation };

        // Act
        var result = service.CanReserve(conflictingReservation, existingReservations);

        // Assert
        Assert.False(result);
    }

    private static Reservation CreateTestReservation(string id, string startTime, string endTime, string roomId = "room-001")
    {
        var timeSlot = new TimeSlot(DateTime.Parse(startTime), DateTime.Parse(endTime));
        return Reservation.Restore(
            id,
            roomId,
            "user-001",
            "Test Meeting",
            timeSlot,
            new List<string> { "user-001" },
            "confirmed",
            0,
            DateTime.UtcNow,
            DateTime.UtcNow);
    }
}