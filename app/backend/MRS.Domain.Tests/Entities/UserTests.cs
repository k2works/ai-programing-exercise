using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;

namespace MRS.Domain.Tests.Entities;

public class UserTests
{
    [Fact]
    public void CtorValidParametersCreatesUser()
    {
        // Arrange
        var userId = new UserId("user123");
        var name = new Name("田中太郎");
        var password = new Password("password123");

        // Act
        var user = new User(userId, name, password);

        // Assert
        Assert.NotNull(user);
        Assert.Equal(userId, user.UserId);
        Assert.Equal(name, user.Name);
        Assert.Equal(password, user.Password);
        Assert.Equal(UserRole.Member, user.Role); // デフォルトロール
        Assert.True(user.IsActive); // デフォルトでアクティブ
        Assert.True(user.CreatedAt <= DateTime.UtcNow);
        Assert.True(user.UpdatedAt <= DateTime.UtcNow);
        Assert.True(Math.Abs((user.CreatedAt - user.UpdatedAt).TotalMilliseconds) < 1000); // 作成時は同じ値（1秒以内の差）
    }

    [Fact]
    public void CtorWithRoleCreatesUserWithSpecifiedRole()
    {
        // Arrange
        var userId = new UserId("admin001");
        var name = new Name("管理者太郎");
        var password = new Password("adminpass123");

        // Act
        var user = new User(userId, name, password, UserRole.Admin);

        // Assert
        Assert.Equal(UserRole.Admin, user.Role);
    }

    [Theory]
    [InlineData("", "田中太郎", "password123")]
    [InlineData("user123", "", "password123")]
    [InlineData("user123", "田中太郎", "")]
    public void CtorInvalidParametersThrowsArgumentException(string userIdValue, string nameValue, string passwordValue)
    {
        // Act & Assert
        if (string.IsNullOrEmpty(userIdValue))
        {
            Assert.Throws<ArgumentException>(() => new User(new UserId(userIdValue), new Name(nameValue), new Password(passwordValue)));
        }
        else if (string.IsNullOrEmpty(nameValue))
        {
            Assert.Throws<ArgumentException>(() => new User(new UserId(userIdValue), new Name(nameValue), new Password(passwordValue)));
        }
        else if (string.IsNullOrEmpty(passwordValue))
        {
            Assert.Throws<ArgumentException>(() => new User(new UserId(userIdValue), new Name(nameValue), new Password(passwordValue)));
        }
    }

    [Fact]
    public void CtorNullUserIdThrowsArgumentNullException()
    {
        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => new User(null!, new Name("田中太郎"), new Password("password123")));
    }

    [Fact]
    public void CtorNullNameThrowsArgumentNullException()
    {
        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => new User(new UserId("user123"), null!, new Password("password123")));
    }

    [Fact]
    public void CtorNullPasswordThrowsArgumentNullException()
    {
        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => new User(new UserId("user123"), new Name("田中太郎"), null!));
    }

    [Fact]
    public void ChangePasswordUpdatesPasswordAndTimestamp()
    {
        // Arrange
        var user = CreateTestUser();
        var newPassword = new Password("newpassword123");
        var originalUpdatedAt = user.UpdatedAt;

        // 時間の経過をシミュレート
        Thread.Sleep(1);

        // Act
        user.ChangePassword(newPassword);

        // Assert
        Assert.Equal(newPassword, user.Password);
        Assert.True(user.UpdatedAt > originalUpdatedAt);
    }

    [Fact]
    public void ChangePasswordNullPasswordThrowsArgumentNullException()
    {
        // Arrange
        var user = CreateTestUser();

        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => user.ChangePassword(null!));
    }

    [Fact]
    public void ChangeNameUpdatesNameAndTimestamp()
    {
        // Arrange
        var user = CreateTestUser();
        var newName = new Name("佐藤花子");
        var originalUpdatedAt = user.UpdatedAt;

        // 時間の経過をシミュレート
        Thread.Sleep(1);

        // Act
        user.ChangeName(newName);

        // Assert
        Assert.Equal(newName, user.Name);
        Assert.True(user.UpdatedAt > originalUpdatedAt);
    }

    [Fact]
    public void ChangeNameNullNameThrowsArgumentNullException()
    {
        // Arrange
        var user = CreateTestUser();

        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => user.ChangeName(null!));
    }

    [Fact]
    public void VerifyPasswordCorrectPasswordReturnsTrue()
    {
        // Arrange
        const string plainPassword = "password123";
        var user = CreateTestUser(password: plainPassword);

        // Act
        var result = user.VerifyPassword(plainPassword);

        // Assert
        Assert.True(result);
    }

    [Fact]
    public void VerifyPasswordIncorrectPasswordReturnsFalse()
    {
        // Arrange
        var user = CreateTestUser(password: "password123");

        // Act
        var result = user.VerifyPassword("wrongpassword");

        // Assert
        Assert.False(result);
    }

    [Fact]
    public void VerifyPasswordNullPasswordReturnsFalse()
    {
        // Arrange
        var user = CreateTestUser();

        // Act
        var result = user.VerifyPassword(null!);

        // Assert
        Assert.False(result);
    }

    [Fact]
    public void DeactivateUserSetsIsActiveToFalseAndUpdatesTimestamp()
    {
        // Arrange
        var user = CreateTestUser();
        var originalUpdatedAt = user.UpdatedAt;

        // 時間の経過をシミュレート
        Thread.Sleep(1);

        // Act
        user.Deactivate();

        // Assert
        Assert.False(user.IsActive);
        Assert.True(user.UpdatedAt > originalUpdatedAt);
    }

    [Fact]
    public void ActivateUserSetsIsActiveToTrueAndUpdatesTimestamp()
    {
        // Arrange
        var user = CreateTestUser();
        user.Deactivate(); // 一度非アクティブにする
        var originalUpdatedAt = user.UpdatedAt;

        // 時間の経過をシミュレート
        Thread.Sleep(1);

        // Act
        user.Activate();

        // Assert
        Assert.True(user.IsActive);
        Assert.True(user.UpdatedAt > originalUpdatedAt);
    }

    [Fact]
    public void ChangeRoleUpdatesRoleAndTimestamp()
    {
        // Arrange
        var user = CreateTestUser();
        var originalUpdatedAt = user.UpdatedAt;

        // 時間の経過をシミュレート
        Thread.Sleep(1);

        // Act
        user.ChangeRole(UserRole.Admin);

        // Assert
        Assert.Equal(UserRole.Admin, user.Role);
        Assert.True(user.UpdatedAt > originalUpdatedAt);
    }

    [Fact]
    public void EqualsSameUserIdReturnsTrue()
    {
        // Arrange
        var userId = new UserId("user123");
        var user1 = new User(userId, new Name("田中太郎"), new Password("password123"));
        var user2 = new User(userId, new Name("佐藤花子"), new Password("password456")); // 異なる名前・パスワード

        // Act & Assert
        Assert.Equal(user1, user2); // UserIdが同じなら等価
        Assert.True(user1.Equals(user2));
        Assert.True(user1 == user2);
        Assert.False(user1 != user2);
    }

    [Fact]
    public void EqualsDifferentUserIdReturnsFalse()
    {
        // Arrange
        var user1 = new User(new UserId("user123"), new Name("田中太郎"), new Password("password123"));
        var user2 = new User(new UserId("user456"), new Name("田中太郎"), new Password("password123"));

        // Act & Assert
        Assert.NotEqual(user1, user2);
        Assert.False(user1.Equals(user2));
        Assert.False(user1 == user2);
        Assert.True(user1 != user2);
    }

    [Fact]
    public void GetHashCodeSameUserIdReturnsSameHashCode()
    {
        // Arrange
        var userId = new UserId("user123");
        var user1 = new User(userId, new Name("田中太郎"), new Password("password123"));
        var user2 = new User(userId, new Name("佐藤花子"), new Password("password456"));

        // Act & Assert
        Assert.Equal(user1.GetHashCode(), user2.GetHashCode());
    }

    [Fact]
    public void ToStringReturnsUserInfo()
    {
        // Arrange
        var user = CreateTestUser(userId: "user123", name: "田中太郎");

        // Act
        var result = user.ToString();

        // Assert
        Assert.Contains("user123", result);
        Assert.Contains("田中太郎", result);
        Assert.DoesNotContain("password", result, StringComparison.OrdinalIgnoreCase); // パスワード情報は含まない
    }

    private static User CreateTestUser(string userId = "testuser", string name = "テストユーザー", string password = "testpass123")
    {
        return new User(
            new UserId(userId),
            new Name(name),
            new Password(password)
        );
    }
}
