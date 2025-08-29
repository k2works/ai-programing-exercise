using MRS.Domain.ValueObjects;

namespace MRS.Domain.Tests.ValueObjects;

public class RoomIdTests
{
    [Fact]
    public void CtorValidValueCreatesRoomId()
    {
        // Arrange
        const string validRoomId = "room001";

        // Act
        var roomId = new RoomId(validRoomId);

        // Assert
        Assert.NotNull(roomId);
        Assert.Equal(validRoomId, roomId.Value);
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    public void CtorInvalidValueThrowsArgumentException(string invalidValue)
    {
        // Act & Assert
        Assert.Throws<ArgumentException>(() => new RoomId(invalidValue));
    }

    [Fact]
    public void CtorNullValueThrowsArgumentException()
    {
        // Act & Assert
        Assert.Throws<ArgumentException>(() => new RoomId(null!));
    }

    [Fact]
    public void CtorValueTooLongThrowsArgumentException()
    {
        // Arrange
        var tooLongValue = new string('a', 51); // 51文字（上限50文字を超過）

        // Act & Assert
        Assert.Throws<ArgumentException>(() => new RoomId(tooLongValue));
    }

    [Fact]
    public void EqualsSameValueReturnsTrue()
    {
        // Arrange
        var roomId1 = new RoomId("room001");
        var roomId2 = new RoomId("room001");

        // Act & Assert
        Assert.Equal(roomId1, roomId2);
        Assert.True(roomId1.Equals(roomId2));
        Assert.True(roomId1 == roomId2);
        Assert.False(roomId1 != roomId2);
    }

    [Fact]
    public void EqualsDifferentValueReturnsFalse()
    {
        // Arrange
        var roomId1 = new RoomId("room001");
        var roomId2 = new RoomId("room002");

        // Act & Assert
        Assert.NotEqual(roomId1, roomId2);
        Assert.False(roomId1.Equals(roomId2));
        Assert.False(roomId1 == roomId2);
        Assert.True(roomId1 != roomId2);
    }

    [Fact]
    public void ToStringReturnsValue()
    {
        // Arrange
        const string value = "room001";
        var roomId = new RoomId(value);

        // Act
        var result = roomId.ToString();

        // Assert
        Assert.Equal(value, result);
    }

    [Fact]
    public void ImplicitStringConversionReturnsValue()
    {
        // Arrange
        const string value = "room001";
        var roomId = new RoomId(value);

        // Act
        string result = roomId; // 暗黙的型変換

        // Assert
        Assert.Equal(value, result);
    }
}
