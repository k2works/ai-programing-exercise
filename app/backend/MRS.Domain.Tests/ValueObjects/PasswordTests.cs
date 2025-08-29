using MRS.Domain.ValueObjects;

namespace MRS.Domain.Tests.ValueObjects;

public class PasswordTests
{
    [Fact]
    public void CtorValidPasswordCreatesPasswordWithHash()
    {
        // Arrange
        const string validPassword = "password123";

        // Act
        var password = new Password(validPassword);

        // Assert
        Assert.NotNull(password);
        Assert.NotNull(password.HashedValue);
        Assert.NotEqual(validPassword, password.HashedValue); // ハッシュ化されている
        Assert.StartsWith("$2a$", password.HashedValue); // BCryptフォーマット
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    public void CtorInvalidPasswordThrowsArgumentException(string invalidPassword)
    {
        // Act & Assert
        Assert.Throws<ArgumentException>(() => new Password(invalidPassword));
    }

    [Fact]
    public void CtorNullPasswordThrowsArgumentException()
    {
        // Act & Assert
        Assert.Throws<ArgumentException>(() => new Password(null!));
    }

    [Fact]
    public void CtorPasswordTooShortThrowsArgumentException()
    {
        // Arrange
        const string shortPassword = "1234567"; // 7文字（最小8文字未満）

        // Act & Assert
        Assert.Throws<ArgumentException>(() => new Password(shortPassword));
    }

    [Fact]
    public void CtorPasswordTooLongThrowsArgumentException()
    {
        // Arrange
        var longPassword = new string('a', 129); // 129文字（上限128文字を超過）

        // Act & Assert
        Assert.Throws<ArgumentException>(() => new Password(longPassword));
    }

    [Fact]
    public void VerifyCorrectPasswordReturnsTrue()
    {
        // Arrange
        const string plainPassword = "password123";
        var password = new Password(plainPassword);

        // Act
        var result = password.Verify(plainPassword);

        // Assert
        Assert.True(result);
    }

    [Fact]
    public void VerifyIncorrectPasswordReturnsFalse()
    {
        // Arrange
        const string plainPassword = "password123";
        const string wrongPassword = "wrongpassword";
        var password = new Password(plainPassword);

        // Act
        var result = password.Verify(wrongPassword);

        // Assert
        Assert.False(result);
    }

    [Fact]
    public void VerifyNullPasswordReturnsFalse()
    {
        // Arrange
        var password = new Password("password123");

        // Act
        var result = password.Verify(null!);

        // Assert
        Assert.False(result);
    }

    [Fact]
    public void EqualsSameHashReturnsTrue()
    {
        // Arrange
        const string plainPassword = "password123";
        var password1 = new Password(plainPassword);
        var password2 = Password.FromHash(password1.HashedValue);

        // Act & Assert
        Assert.Equal(password1, password2);
        Assert.True(password1.Equals(password2));
        Assert.True(password1 == password2);
        Assert.False(password1 != password2);
    }

    [Fact]
    public void EqualsDifferentHashReturnsFalse()
    {
        // Arrange
        var password1 = new Password("password123");
        var password2 = new Password("password456");

        // Act & Assert
        Assert.NotEqual(password1, password2);
        Assert.False(password1.Equals(password2));
        Assert.False(password1 == password2);
        Assert.True(password1 != password2);
    }

    [Fact]
    public void FromHashValidHashCreatesPassword()
    {
        // Arrange
        const string validHash = "$2a$11$abcdefghijklmnopqrstuvwxyz"; // 仮のBCryptハッシュ

        // Act
        var password = Password.FromHash(validHash);

        // Assert
        Assert.NotNull(password);
        Assert.Equal(validHash, password.HashedValue);
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    [InlineData("invalidhash")]
    public void FromHashInvalidHashThrowsArgumentException(string invalidHash)
    {
        // Act & Assert
        Assert.Throws<ArgumentException>(() => Password.FromHash(invalidHash));
    }

    [Fact]
    public void FromHashNullHashThrowsArgumentException()
    {
        // Act & Assert
        Assert.Throws<ArgumentException>(() => Password.FromHash(null!));
    }

    [Fact]
    public void GetHashCodeSameHashReturnsSameHashCode()
    {
        // Arrange
        const string plainPassword = "password123";
        var password1 = new Password(plainPassword);
        var password2 = Password.FromHash(password1.HashedValue);

        // Act & Assert
        Assert.Equal(password1.GetHashCode(), password2.GetHashCode());
    }

    [Fact]
    public void ToStringReturnsMaskedValue()
    {
        // Arrange
        var password = new Password("password123");

        // Act
        var result = password.ToString();

        // Assert
        Assert.Equal("********", result);
    }
}
