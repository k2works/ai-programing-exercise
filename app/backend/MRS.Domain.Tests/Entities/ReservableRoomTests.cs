using Xunit;
using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;

namespace MRS.Domain.Tests.Entities;

/// <summary>
/// ReservableRoomエンティティのテスト
/// </summary>
public class ReservableRoomTests
{
    private readonly ReservableRoomId _reservableRoomId = new("RESROOM001");
    private readonly RoomId _roomId = new("ROOM001");
    private readonly Name _roomName = new("会議室A");

    [Fact]
    public void Constructor_ValidParameters_ShouldCreateInstance()
    {
        // Act
        var reservableRoom = new ReservableRoom(_reservableRoomId, _roomId, _roomName);

        // Assert
        Assert.NotNull(reservableRoom);
        Assert.Equal(_reservableRoomId, reservableRoom.ReservableRoomId);
        Assert.Equal(_roomId, reservableRoom.RoomId);
        Assert.Equal(_roomName, reservableRoom.RoomName);
        Assert.True(reservableRoom.IsAvailable);
        Assert.True(reservableRoom.CreatedAt <= DateTime.UtcNow);
        Assert.True(reservableRoom.UpdatedAt <= DateTime.UtcNow);
        // 時間の精度の問題を避けるため、差が1秒以内であることを確認
        Assert.True(Math.Abs((reservableRoom.CreatedAt - reservableRoom.UpdatedAt).TotalMilliseconds) < 1000);
    }

    [Fact]
    public void Constructor_NullReservableRoomId_ShouldThrowArgumentNullException()
    {
        // Act & Assert
        var exception = Assert.Throws<ArgumentNullException>(() => new ReservableRoom(null!, _roomId, _roomName));
        Assert.Equal("reservableRoomId", exception.ParamName);
    }

    [Fact]
    public void Constructor_NullRoomId_ShouldThrowArgumentNullException()
    {
        // Act & Assert
        var exception = Assert.Throws<ArgumentNullException>(() => new ReservableRoom(_reservableRoomId, null!, _roomName));
        Assert.Equal("roomId", exception.ParamName);
    }

    [Fact]
    public void Constructor_NullRoomName_ShouldThrowArgumentNullException()
    {
        // Act & Assert
        var exception = Assert.Throws<ArgumentNullException>(() => new ReservableRoom(_reservableRoomId, _roomId, null!));
        Assert.Equal("roomName", exception.ParamName);
    }

    [Fact]
    public void ChangeRoomName_ValidName_ShouldUpdateRoomNameAndUpdatedAt()
    {
        // Arrange
        var reservableRoom = new ReservableRoom(_reservableRoomId, _roomId, _roomName);
        var originalUpdatedAt = reservableRoom.UpdatedAt;
        var newRoomName = new Name("会議室B");
        
        // Wait to ensure UpdatedAt changes
        Thread.Sleep(1);

        // Act
        reservableRoom.ChangeRoomName(newRoomName);

        // Assert
        Assert.Equal(newRoomName, reservableRoom.RoomName);
        Assert.True(reservableRoom.UpdatedAt > originalUpdatedAt);
    }

    [Fact]
    public void ChangeRoomName_NullRoomName_ShouldThrowArgumentNullException()
    {
        // Arrange
        var reservableRoom = new ReservableRoom(_reservableRoomId, _roomId, _roomName);

        // Act & Assert
        var exception = Assert.Throws<ArgumentNullException>(() => reservableRoom.ChangeRoomName(null!));
        Assert.Equal("newRoomName", exception.ParamName);
    }

    [Fact]
    public void MakeUnavailable_ShouldSetIsAvailableToFalseAndUpdateTimestamp()
    {
        // Arrange
        var reservableRoom = new ReservableRoom(_reservableRoomId, _roomId, _roomName);
        var originalUpdatedAt = reservableRoom.UpdatedAt;
        
        // Wait to ensure UpdatedAt changes
        Thread.Sleep(1);

        // Act
        reservableRoom.MakeUnavailable();

        // Assert
        Assert.False(reservableRoom.IsAvailable);
        Assert.True(reservableRoom.UpdatedAt > originalUpdatedAt);
    }

    [Fact]
    public void MakeAvailable_ShouldSetIsAvailableToTrueAndUpdateTimestamp()
    {
        // Arrange
        var reservableRoom = new ReservableRoom(_reservableRoomId, _roomId, _roomName);
        reservableRoom.MakeUnavailable();
        var originalUpdatedAt = reservableRoom.UpdatedAt;
        
        // Wait to ensure UpdatedAt changes
        Thread.Sleep(1);

        // Act
        reservableRoom.MakeAvailable();

        // Assert
        Assert.True(reservableRoom.IsAvailable);
        Assert.True(reservableRoom.UpdatedAt > originalUpdatedAt);
    }

    [Fact]
    public void IsAvailableForReservation_WhenAvailable_ShouldReturnTrue()
    {
        // Arrange
        var reservableRoom = new ReservableRoom(_reservableRoomId, _roomId, _roomName);

        // Act
        var result = reservableRoom.IsAvailableForReservation();

        // Assert
        Assert.True(result);
    }

    [Fact]
    public void IsAvailableForReservation_WhenUnavailable_ShouldReturnFalse()
    {
        // Arrange
        var reservableRoom = new ReservableRoom(_reservableRoomId, _roomId, _roomName);
        reservableRoom.MakeUnavailable();

        // Act
        var result = reservableRoom.IsAvailableForReservation();

        // Assert
        Assert.False(result);
    }

    [Fact]
    public void Equals_SameReservableRoomId_ShouldReturnTrue()
    {
        // Arrange
        var reservableRoom1 = new ReservableRoom(_reservableRoomId, _roomId, _roomName);
        var reservableRoom2 = new ReservableRoom(_reservableRoomId, new RoomId("ROOM002"), new Name("会議室B"));

        // Act & Assert
        Assert.True(reservableRoom1.Equals(reservableRoom2));
        Assert.True(reservableRoom1 == reservableRoom2);
        Assert.False(reservableRoom1 != reservableRoom2);
    }

    [Fact]
    public void Equals_DifferentReservableRoomId_ShouldReturnFalse()
    {
        // Arrange
        var reservableRoom1 = new ReservableRoom(_reservableRoomId, _roomId, _roomName);
        var reservableRoom2 = new ReservableRoom(new ReservableRoomId("RESROOM002"), _roomId, _roomName);

        // Act & Assert
        Assert.False(reservableRoom1.Equals(reservableRoom2));
        Assert.False(reservableRoom1 == reservableRoom2);
        Assert.True(reservableRoom1 != reservableRoom2);
    }

    [Fact]
    public void Equals_Null_ShouldReturnFalse()
    {
        // Arrange
        var reservableRoom = new ReservableRoom(_reservableRoomId, _roomId, _roomName);

        // Act & Assert
        Assert.False(reservableRoom.Equals(null));
        Assert.False(reservableRoom == null);
        Assert.True(reservableRoom != null);
    }

    [Fact]
    public void GetHashCode_SameReservableRoomId_ShouldReturnSameHashCode()
    {
        // Arrange
        var reservableRoom1 = new ReservableRoom(_reservableRoomId, _roomId, _roomName);
        var reservableRoom2 = new ReservableRoom(_reservableRoomId, new RoomId("ROOM002"), new Name("会議室B"));

        // Act
        var hashCode1 = reservableRoom1.GetHashCode();
        var hashCode2 = reservableRoom2.GetHashCode();

        // Assert
        Assert.Equal(hashCode1, hashCode2);
    }

    [Fact]
    public void ToString_ShouldReturnFormattedString()
    {
        // Arrange
        var reservableRoom = new ReservableRoom(_reservableRoomId, _roomId, _roomName);

        // Act
        var result = reservableRoom.ToString();

        // Assert
        Assert.Contains("RESROOM001", result);
        Assert.Contains("ROOM001", result);
        Assert.Contains("会議室A", result);
        Assert.Contains("True", result); // IsAvailable
    }
}
