using MRS.Domain.ValueObjects;

namespace MRS.Domain.Tests;

public class UserIdTests
{
    [Fact]
    public void CtorValidValueCreatesUserId()
    {
        // Arrange
        const string validUserId = "user123";

        // Act
        var userId = new UserId(validUserId);

        // Assert
        Assert.NotNull(userId);
        Assert.Equal(validUserId, userId.Value);
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    public void CtorInvalidValueThrowsArgumentException(string invalidValue)
    {
        // Act & Assert
        Assert.Throws<ArgumentException>(() => new UserId(invalidValue));
    }

    [Fact]
    public void CtorNullValueThrowsArgumentException()
    {
        // Act & Assert
        Assert.Throws<ArgumentException>(() => new UserId(null!));
    }

    [Fact]
    public void CtorValueTooLongThrowsArgumentException()
    {
        // Arrange
        var tooLongValue = new string('a', 51); // 51文字（上限50文字を超過）

        // Act & Assert
        Assert.Throws<ArgumentException>(() => new UserId(tooLongValue));
    }

    [Fact]
    public void EqualsSameValueReturnsTrue()
    {
        // Arrange
        var userId1 = new UserId("user123");
        var userId2 = new UserId("user123");

        // Act & Assert
        Assert.Equal(userId1, userId2);
        Assert.True(userId1.Equals(userId2));
        Assert.True(userId1 == userId2);
        Assert.False(userId1 != userId2);
    }

    [Fact]
    public void EqualsDifferentValueReturnsFalse()
    {
        // Arrange
        var userId1 = new UserId("user123");
        var userId2 = new UserId("user456");

        // Act & Assert
        Assert.NotEqual(userId1, userId2);
        Assert.False(userId1.Equals(userId2));
        Assert.False(userId1 == userId2);
        Assert.True(userId1 != userId2);
    }

    [Fact]
    public void GetHashCodeSameValueReturnsSameHashCode()
    {
        // Arrange
        var userId1 = new UserId("user123");
        var userId2 = new UserId("user123");

        // Act & Assert
        Assert.Equal(userId1.GetHashCode(), userId2.GetHashCode());
    }

    [Fact]
    public void ToStringReturnsValue()
    {
        // Arrange
        const string value = "user123";
        var userId = new UserId(value);

        // Act
        var result = userId.ToString();

        // Assert
        Assert.Equal(value, result);
    }
}
