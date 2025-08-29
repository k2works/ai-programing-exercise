using MRS.Domain.ValueObjects;

namespace MRS.Domain.Tests.ValueObjects;

public class NameTests
{
    [Fact]
    public void CtorValidNameCreatesName()
    {
        // Arrange
        const string validName = "田中太郎";

        // Act
        var name = new Name(validName);

        // Assert
        Assert.NotNull(name);
        Assert.Equal(validName, name.Value);
    }

    [Fact]
    public void CtorNameWithWhitespaceTrimsAndCreatesName()
    {
        // Arrange
        const string nameWithWhitespace = "  田中太郎  ";
        const string expectedName = "田中太郎";

        // Act
        var name = new Name(nameWithWhitespace);

        // Assert
        Assert.Equal(expectedName, name.Value);
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    public void CtorInvalidNameThrowsArgumentException(string invalidName)
    {
        // Act & Assert
        Assert.Throws<ArgumentException>(() => new Name(invalidName));
    }

    [Fact]
    public void CtorNullNameThrowsArgumentException()
    {
        // Act & Assert
        Assert.Throws<ArgumentException>(() => new Name(null!));
    }

    [Fact]
    public void CtorNameTooLongThrowsArgumentException()
    {
        // Arrange
        var longName = new string('あ', 101); // 101文字（上限100文字を超過）

        // Act & Assert
        Assert.Throws<ArgumentException>(() => new Name(longName));
    }

    [Fact]
    public void CtorSingleCharacterNameCreatesName()
    {
        // Arrange
        const string singleChar = "A";

        // Act
        var name = new Name(singleChar);

        // Assert
        Assert.Equal(singleChar, name.Value);
    }

    [Fact]
    public void CtorMaxLengthNameCreatesName()
    {
        // Arrange
        var maxLengthName = new string('あ', 100); // 100文字（上限）

        // Act
        var name = new Name(maxLengthName);

        // Assert
        Assert.Equal(maxLengthName, name.Value);
    }

    [Fact]
    public void EqualsSameValueReturnsTrue()
    {
        // Arrange
        var name1 = new Name("田中太郎");
        var name2 = new Name("田中太郎");

        // Act & Assert
        Assert.Equal(name1, name2);
        Assert.True(name1.Equals(name2));
        Assert.True(name1 == name2);
        Assert.False(name1 != name2);
    }

    [Fact]
    public void EqualsDifferentValueReturnsFalse()
    {
        // Arrange
        var name1 = new Name("田中太郎");
        var name2 = new Name("佐藤花子");

        // Act & Assert
        Assert.NotEqual(name1, name2);
        Assert.False(name1.Equals(name2));
        Assert.False(name1 == name2);
        Assert.True(name1 != name2);
    }

    [Fact]
    public void EqualsCaseInsensitiveReturnsFalse()
    {
        // Arrange
        var name1 = new Name("Tanaka Taro");
        var name2 = new Name("tanaka taro");

        // Act & Assert
        Assert.NotEqual(name1, name2); // 大文字小文字は区別する
    }

    [Fact]
    public void EqualsTrimmingConsistencyReturnsTrue()
    {
        // Arrange
        var name1 = new Name("田中太郎");
        var name2 = new Name("  田中太郎  "); // トリムされて同じ値になる

        // Act & Assert
        Assert.Equal(name1, name2);
    }

    [Fact]
    public void GetHashCodeSameValueReturnsSameHashCode()
    {
        // Arrange
        var name1 = new Name("田中太郎");
        var name2 = new Name("田中太郎");

        // Act & Assert
        Assert.Equal(name1.GetHashCode(), name2.GetHashCode());
    }

    [Fact]
    public void ToStringReturnsValue()
    {
        // Arrange
        const string value = "田中太郎";
        var name = new Name(value);

        // Act
        var result = name.ToString();

        // Assert
        Assert.Equal(value, result);
    }

    [Fact]
    public void ImplicitStringConversionReturnsValue()
    {
        // Arrange
        const string value = "田中太郎";
        var name = new Name(value);

        // Act
        string result = name; // 暗黙的型変換

        // Assert
        Assert.Equal(value, result);
    }

    [Theory]
    [InlineData("田中", "田中")]
    [InlineData("John Smith", "John Smith")]
    [InlineData("山田　花子", "山田　花子")] // 全角スペース含む
    [InlineData("O'Connor", "O'Connor")] // アポストロフィ含む
    [InlineData("José María", "José María")] // アクセント記号含む
    public void CtorVariousValidNamesCreateName(string input, string expected)
    {
        // Act
        var name = new Name(input);

        // Assert
        Assert.Equal(expected, name.Value);
    }
}
