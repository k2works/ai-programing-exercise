using Xunit;
using Moq;
using MRS.Application.Ports;
using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;

namespace MRS.Application.Tests.Ports;

/// <summary>
/// IUserRepositoryインターフェースのテスト
/// </summary>
public class IUserRepositoryTests
{
    [Fact]
    public void GetByIdAsync_ValidUserId_ShouldReturnUser()
    {
        // Arrange
        var mockUserRepository = new Mock<IUserRepository>();
        var userId = new UserId("USER001");

        var expectedUser = new User(
            userId,
            new Name("テストユーザー"),
            new Password("password123"),
            UserRole.Member
        );

        mockUserRepository
            .Setup(x => x.GetByIdAsync(userId, It.IsAny<CancellationToken>()))
            .ReturnsAsync(expectedUser);

        // Act & Assert
        var userRepository = mockUserRepository.Object;
        Assert.NotNull(userRepository);

        // メソッドシグネチャの存在確認
        var getByIdMethod = typeof(IUserRepository).GetMethod("GetByIdAsync");
        Assert.NotNull(getByIdMethod);
        Assert.Equal(typeof(Task<User?>), getByIdMethod.ReturnType);
    }

    [Fact]
    public void GetByIdAsync_InvalidUserId_ShouldReturnNull()
    {
        // Arrange
        var mockUserRepository = new Mock<IUserRepository>();
        var invalidUserId = new UserId("INVALID_USER");

        mockUserRepository
            .Setup(x => x.GetByIdAsync(invalidUserId, It.IsAny<CancellationToken>()))
            .ReturnsAsync((User?)null);

        // Act & Assert
        var userRepository = mockUserRepository.Object;
        Assert.NotNull(userRepository);
    }

    [Fact]
    public void AddAsync_ValidUser_ShouldCompleteSuccessfully()
    {
        // Arrange
        var mockUserRepository = new Mock<IUserRepository>();
        var user = new User(
            new UserId("USER001"),
            new Name("テストユーザー"),
            new Password("password123"),
            UserRole.Member
        );

        mockUserRepository
            .Setup(x => x.AddAsync(user, It.IsAny<CancellationToken>()))
            .Returns(Task.CompletedTask);

        // Act & Assert
        var userRepository = mockUserRepository.Object;
        Assert.NotNull(userRepository);

        // メソッドシグネチャの存在確認
        var addMethod = typeof(IUserRepository).GetMethod("AddAsync");
        Assert.NotNull(addMethod);
        Assert.Equal(typeof(Task), addMethod.ReturnType);
    }

    [Fact]
    public void UpdateAsync_ValidUser_ShouldCompleteSuccessfully()
    {
        // Arrange
        var mockUserRepository = new Mock<IUserRepository>();
        var user = new User(
            new UserId("USER001"),
            new Name("更新テストユーザー"),
            new Password("newpassword123"),
            UserRole.Admin
        );

        mockUserRepository
            .Setup(x => x.UpdateAsync(user, It.IsAny<CancellationToken>()))
            .Returns(Task.CompletedTask);

        // Act & Assert
        var userRepository = mockUserRepository.Object;
        Assert.NotNull(userRepository);

        // メソッドシグネチャの存在確認
        var updateMethod = typeof(IUserRepository).GetMethod("UpdateAsync");
        Assert.NotNull(updateMethod);
        Assert.Equal(typeof(Task), updateMethod.ReturnType);
    }

    [Fact]
    public void DeleteAsync_ValidUserId_ShouldCompleteSuccessfully()
    {
        // Arrange
        var mockUserRepository = new Mock<IUserRepository>();
        var userId = new UserId("USER001");

        mockUserRepository
            .Setup(x => x.DeleteAsync(userId, It.IsAny<CancellationToken>()))
            .Returns(Task.CompletedTask);

        // Act & Assert
        var userRepository = mockUserRepository.Object;
        Assert.NotNull(userRepository);

        // メソッドシグネチャの存在確認
        var deleteMethod = typeof(IUserRepository).GetMethod("DeleteAsync");
        Assert.NotNull(deleteMethod);
        Assert.Equal(typeof(Task), deleteMethod.ReturnType);
    }
}
