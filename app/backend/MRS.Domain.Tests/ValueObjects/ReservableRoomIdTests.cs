using Xunit;
using MRS.Domain.ValueObjects;

namespace MRS.Domain.Tests.ValueObjects;

/// <summary>
/// ReservableRoomIdバリューオブジェクトのテスト
/// </summary>
public class ReservableRoomIdTests
{
    [Fact]
    public void ConstructorValidValueShouldCreateInstance()
    {
        // Arrange
        var value = "RESROOM001";

        // Act
        var reservableRoomId = new ReservableRoomId(value);

        // Assert
        Assert.Equal(value, reservableRoomId.Value);
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData("   ")]
    public void ConstructorNullOrWhiteSpaceShouldThrowArgumentException(string? value)
    {
        // Act & Assert
        var exception = Assert.Throws<ArgumentException>(() => new ReservableRoomId(value!));
        Assert.Contains("ReservableRoomIdは必須です", exception.Message);
    }

    [Fact]
    public void ConstructorExceedsMaxLengthShouldThrowArgumentException()
    {
        // Arrange
        var longValue = new string('A', 101); // 101文字

        // Act & Assert
        var exception = Assert.Throws<ArgumentException>(() => new ReservableRoomId(longValue));
        Assert.Contains("ReservableRoomIdは100文字以下で入力してください", exception.Message);
    }

    [Fact]
    public void EqualsSameValueShouldReturnTrue()
    {
        // Arrange
        var value = "RESROOM001";
        var reservableRoomId1 = new ReservableRoomId(value);
        var reservableRoomId2 = new ReservableRoomId(value);

        // Act & Assert
        Assert.True(reservableRoomId1.Equals(reservableRoomId2));
        Assert.True(reservableRoomId1 == reservableRoomId2);
        Assert.False(reservableRoomId1 != reservableRoomId2);
    }

    [Fact]
    public void EqualsDifferentValueShouldReturnFalse()
    {
        // Arrange
        var reservableRoomId1 = new ReservableRoomId("RESROOM001");
        var reservableRoomId2 = new ReservableRoomId("RESROOM002");

        // Act & Assert
        Assert.False(reservableRoomId1.Equals(reservableRoomId2));
        Assert.False(reservableRoomId1 == reservableRoomId2);
        Assert.True(reservableRoomId1 != reservableRoomId2);
    }

    [Fact]
    public void GetHashCodeSameValueShouldReturnSameHashCode()
    {
        // Arrange
        var value = "RESROOM001";
        var reservableRoomId1 = new ReservableRoomId(value);
        var reservableRoomId2 = new ReservableRoomId(value);

        // Act
        var hashCode1 = reservableRoomId1.GetHashCode();
        var hashCode2 = reservableRoomId2.GetHashCode();

        // Assert
        Assert.Equal(hashCode1, hashCode2);
    }

    [Fact]
    public void ToStringShouldReturnValue()
    {
        // Arrange
        var value = "RESROOM001";
        var reservableRoomId = new ReservableRoomId(value);

        // Act
        var result = reservableRoomId.ToString();

        // Assert
        Assert.Equal(value, result);
    }

    [Fact]
    public void ImplicitConversionToStringFromReservableRoomIdShouldReturnValue()
    {
        // Arrange
        var reservableRoomId = new ReservableRoomId("RESROOM001");

        // Act
        string value = reservableRoomId;

        // Assert
        Assert.Equal("RESROOM001", value);
    }
}
