using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;

namespace MRS.Domain.Tests.Entities;

public class ReservationTests
{
    [Fact]
    public void Create_WithValidParameters_CreatesReservation()
    {
        // Arrange
        var roomId = "room1";
        var userId = "user1";
        var title = "Test Meeting";
        var timeSlot = new TimeSlot(DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));
        var participants = new List<string> { "participant1", "participant2" };

        // Act
        var reservation = Reservation.Create(roomId, userId, title, timeSlot, participants);

        // Assert
        Assert.Equal(roomId, reservation.RoomId);
        Assert.Equal(userId, reservation.UserId);
        Assert.Equal(title, reservation.Title);
        Assert.Equal(timeSlot, reservation.TimeSlot);
        Assert.Equal(participants.Count, reservation.Participants.Count);
        Assert.Equal(ReservationStatus.Confirmed, reservation.Status);
        Assert.Equal(0, reservation.RowVersion);
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData(" ")]
    public void Create_WithInvalidRoomId_ThrowsArgumentException(string? roomId)
    {
        // Arrange
        var userId = "user1";
        var title = "Test Meeting";
        var timeSlot = new TimeSlot(DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));
        var participants = new List<string>();

        // Act & Assert
        Assert.Throws<ArgumentException>(() => Reservation.Create(roomId!, userId, title, timeSlot, participants));
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData(" ")]
    public void Create_WithInvalidUserId_ThrowsArgumentException(string? userId)
    {
        // Arrange
        var roomId = "room1";
        var title = "Test Meeting";
        var timeSlot = new TimeSlot(DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));
        var participants = new List<string>();

        // Act & Assert
        Assert.Throws<ArgumentException>(() => Reservation.Create(roomId, userId!, title, timeSlot, participants));
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData(" ")]
    public void Create_WithInvalidTitle_ThrowsArgumentException(string? title)
    {
        // Arrange
        var roomId = "room1";
        var userId = "user1";
        var timeSlot = new TimeSlot(DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));
        var participants = new List<string>();

        // Act & Assert
        Assert.Throws<ArgumentException>(() => Reservation.Create(roomId, userId, title!, timeSlot, participants));
    }

    [Fact]
    public void Create_WithNullTimeSlot_ThrowsArgumentNullException()
    {
        // Arrange
        var roomId = "room1";
        var userId = "user1";
        var title = "Test Meeting";
        var participants = new List<string>();

        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => Reservation.Create(roomId, userId, title, null!, participants));
    }

    [Fact]
    public void SetReservationId_WithValidId_SetsReservationId()
    {
        // Arrange
        var reservation = CreateTestReservation();
        var reservationId = "test-id";

        // Act
        reservation.SetReservationId(reservationId);

        // Assert
        Assert.Equal(reservationId, reservation.ReservationId);
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData(" ")]
    public void SetReservationId_WithInvalidId_ThrowsArgumentException(string? reservationId)
    {
        // Arrange
        var reservation = CreateTestReservation();

        // Act & Assert
        Assert.Throws<ArgumentException>(() => reservation.SetReservationId(reservationId!));
    }

    [Fact]
    public void Update_WithValidParameters_UpdatesReservation()
    {
        // Arrange
        var reservation = CreateTestReservation();
        var newTitle = "Updated Meeting";
        var newTimeSlot = new TimeSlot(DateTime.UtcNow.AddHours(2), DateTime.UtcNow.AddHours(3));
        var newParticipants = new List<string> { "new-participant" };

        // Act
        reservation.Update(newTitle, newTimeSlot, newParticipants);

        // Assert
        Assert.Equal(newTitle, reservation.Title);
        Assert.Equal(newTimeSlot, reservation.TimeSlot);
        Assert.Single(reservation.Participants);
        Assert.Equal("new-participant", reservation.Participants[0]);
        Assert.Equal(1, reservation.RowVersion);
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData(" ")]
    public void Update_WithInvalidTitle_ThrowsArgumentException(string? title)
    {
        // Arrange
        var reservation = CreateTestReservation();
        var timeSlot = new TimeSlot(DateTime.UtcNow.AddHours(2), DateTime.UtcNow.AddHours(3));
        var participants = new List<string>();

        // Act & Assert
        Assert.Throws<ArgumentException>(() => reservation.Update(title!, timeSlot, participants));
    }

    [Fact]
    public void Update_WithNullTimeSlot_ThrowsArgumentNullException()
    {
        // Arrange
        var reservation = CreateTestReservation();
        var title = "Updated Meeting";
        var participants = new List<string>();

        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => reservation.Update(title, null!, participants));
    }

    [Fact]
    public void Cancel_UpdatesStatusAndRowVersion()
    {
        // Arrange
        var reservation = CreateTestReservation();

        // Act
        reservation.Cancel();

        // Assert
        Assert.Equal(ReservationStatus.Cancelled, reservation.Status);
        Assert.Equal(1, reservation.RowVersion);
    }

    [Fact]
    public void IsConflictWith_WithOverlappingTimeSlot_ReturnsTrue()
    {
        // Arrange
        var originalTimeSlot = new TimeSlot(DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));
        var reservation = Reservation.Create("room1", "user1", "Test Meeting", originalTimeSlot, new List<string>());

        var overlappingTimeSlot = new TimeSlot(DateTime.UtcNow.AddHours(1).AddMinutes(30), DateTime.UtcNow.AddHours(2).AddMinutes(30));

        // Act
        var result = reservation.IsConflictWith(overlappingTimeSlot);

        // Assert
        Assert.True(result);
    }

    [Fact]
    public void IsConflictWith_WithNonOverlappingTimeSlot_ReturnsFalse()
    {
        // Arrange
        var originalTimeSlot = new TimeSlot(DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));
        var reservation = Reservation.Create("room1", "user1", "Test Meeting", originalTimeSlot, new List<string>());

        var nonOverlappingTimeSlot = new TimeSlot(DateTime.UtcNow.AddHours(3), DateTime.UtcNow.AddHours(4));

        // Act
        var result = reservation.IsConflictWith(nonOverlappingTimeSlot);

        // Assert
        Assert.False(result);
    }

    [Fact]
    public void IsConflictWith_WhenReservationIsCancelled_ReturnsFalse()
    {
        // Arrange
        var originalTimeSlot = new TimeSlot(DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));
        var reservation = Reservation.Create("room1", "user1", "Test Meeting", originalTimeSlot, new List<string>());
        reservation.Cancel();

        var overlappingTimeSlot = new TimeSlot(DateTime.UtcNow.AddHours(1).AddMinutes(30), DateTime.UtcNow.AddHours(2).AddMinutes(30));

        // Act
        var result = reservation.IsConflictWith(overlappingTimeSlot);

        // Assert
        Assert.False(result); // Cancelled reservations don't conflict
    }

    [Fact]
    public void Restore_WithValidParameters_RestoresReservation()
    {
        // Arrange
        var reservationId = "test-id";
        var roomId = "room1";
        var userId = "user1";
        var title = "Test Meeting";
        var timeSlot = new TimeSlot(DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));
        var participants = new List<string> { "participant1" };
        var status = "Confirmed";
        var rowVersion = 5;
        var createdAt = DateTime.UtcNow.AddDays(-1);
        var updatedAt = DateTime.UtcNow;

        // Act
        var reservation = Reservation.Restore(reservationId, roomId, userId, title, timeSlot, participants, status, rowVersion, createdAt, updatedAt);

        // Assert
        Assert.Equal(reservationId, reservation.ReservationId);
        Assert.Equal(roomId, reservation.RoomId);
        Assert.Equal(userId, reservation.UserId);
        Assert.Equal(title, reservation.Title);
        Assert.Equal(timeSlot, reservation.TimeSlot);
        Assert.Single(reservation.Participants);
        Assert.Equal("participant1", reservation.Participants[0]);
        Assert.Equal(ReservationStatus.Confirmed, reservation.Status);
        Assert.Equal(rowVersion, reservation.RowVersion);
        Assert.Equal(createdAt, reservation.CreatedAt);
        Assert.Equal(updatedAt, reservation.UpdatedAt);
    }

    private static Reservation CreateTestReservation()
    {
        var timeSlot = new TimeSlot(DateTime.UtcNow.AddHours(1), DateTime.UtcNow.AddHours(2));
        return Reservation.Create("room1", "user1", "Test Meeting", timeSlot, new List<string>());
    }
}