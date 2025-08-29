using Microsoft.EntityFrameworkCore;
using Xunit;
using MRS.Infrastructure.Data;
using MRS.Infrastructure.Repositories;
using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;

namespace MRS.Infrastructure.Tests.Repositories;

/// <summary>
/// UserRepositoryのテスト
/// </summary>
public class UserRepositoryTests : IDisposable
{
    private readonly ApplicationDbContext _context;
    private readonly UserRepository _userRepository;

    public UserRepositoryTests()
    {
        var options = new DbContextOptionsBuilder<ApplicationDbContext>()
            .UseInMemoryDatabase(databaseName: Guid.NewGuid().ToString())
            .Options;

        _context = new ApplicationDbContext(options);
        _userRepository = new UserRepository(_context);
    }

    [Fact]
    public async Task GetByIdAsync_ExistingUser_ShouldReturnUser()
    {
        // Arrange
        var userId = new UserId("user001");
        var userName = new Name("山田太郎");
        var password = new Password("Password123!");
        
        var user = new User(userId, userName, password);
        await _userRepository.AddAsync(user);
        await _context.SaveChangesAsync();

        // Act
        var result = await _userRepository.GetByIdAsync(userId);

        // Assert
        Assert.NotNull(result);
        Assert.Equal(userId, result.UserId);
        Assert.Equal(userName, result.Name);
        Assert.True(result.IsActive);
    }

    [Fact]
    public async Task GetByIdAsync_NonExistingUser_ShouldReturnNull()
    {
        // Arrange
        var userId = new UserId("nonexistent");

        // Act
        var result = await _userRepository.GetByIdAsync(userId);

        // Assert
        Assert.Null(result);
    }

    [Fact]
    public async Task AddAsync_ValidUser_ShouldAddToDatabase()
    {
        // Arrange
        var userId = new UserId("user002");
        var userName = new Name("佐藤花子");
        var password = new Password("Password456!");
        
        var user = new User(userId, userName, password);

        // Act
        await _userRepository.AddAsync(user);
        await _context.SaveChangesAsync();

        // Assert
        var savedUser = await _context.Users
            .FirstOrDefaultAsync(u => u.UserId == userId);
        Assert.NotNull(savedUser);
        Assert.Equal(userId, savedUser.UserId);
        Assert.Equal(userName, savedUser.Name);
    }

    [Fact]
    public async Task UpdateAsync_ExistingUser_ShouldUpdateDatabase()
    {
        // Arrange
        var userId = new UserId("user003");
        var userName = new Name("田中一郎");
        var password = new Password("Password789!");
        
        var user = new User(userId, userName, password);
        await _userRepository.AddAsync(user);
        await _context.SaveChangesAsync();

        // 名前を変更
        var newUserName = new Name("田中二郎");
        user.ChangeName(newUserName);

        // Act
        await _userRepository.UpdateAsync(user);
        await _context.SaveChangesAsync();

        // Assert
        var updatedUser = await _userRepository.GetByIdAsync(userId);
        Assert.NotNull(updatedUser);
        Assert.Equal(newUserName, updatedUser.Name);
    }

    [Fact]
    public async Task DeleteAsync_ExistingUser_ShouldRemoveFromDatabase()
    {
        // Arrange
        var userId = new UserId("user004");
        var userName = new Name("鈴木三郎");
        var password = new Password("Password321!");
        
        var user = new User(userId, userName, password);
        await _userRepository.AddAsync(user);
        await _context.SaveChangesAsync();

        // Act
        await _userRepository.DeleteAsync(userId);
        await _context.SaveChangesAsync();

        // Assert
        var deletedUser = await _userRepository.GetByIdAsync(userId);
        Assert.Null(deletedUser);
    }

    [Fact]
    public async Task GetByNameAsync_ExistingName_ShouldReturnUser()
    {
        // Arrange
        var userId = new UserId("user005");
        var userName = new Name("高橋四郎");
        var password = new Password("Password654!");
        
        var user = new User(userId, userName, password);
        await _userRepository.AddAsync(user);
        await _context.SaveChangesAsync();

        // Act
        var result = await _userRepository.GetByNameAsync(userName);

        // Assert
        Assert.NotNull(result);
        Assert.Equal(userName, result.Name);
        Assert.Equal(userId, result.UserId);
    }

    [Fact]
    public async Task GetByNameAsync_NonExistingName_ShouldReturnNull()
    {
        // Arrange
        var userName = new Name("存在しないユーザー");

        // Act
        var result = await _userRepository.GetByNameAsync(userName);

        // Assert
        Assert.Null(result);
    }

    [Fact]
    public async Task GetAllAsync_MultipleUsers_ShouldReturnAllUsers()
    {
        // Arrange
        var users = new List<User>
        {
            new(new UserId("user006"), new Name("ユーザー1"), new Password("Password1!")),
            new(new UserId("user007"), new Name("ユーザー2"), new Password("Password2!")),
            new(new UserId("user008"), new Name("ユーザー3"), new Password("Password3!"))
        };

        foreach (var user in users)
        {
            await _userRepository.AddAsync(user);
        }
        await _context.SaveChangesAsync();

        // Act
        var result = await _userRepository.GetAllAsync();

        // Assert
        var resultList = result.ToList();
        Assert.Equal(3, resultList.Count);
        Assert.Contains(resultList, u => u.UserId.Value == "user006");
        Assert.Contains(resultList, u => u.UserId.Value == "user007");
        Assert.Contains(resultList, u => u.UserId.Value == "user008");
    }

    public void Dispose()
    {
        _context.Dispose();
    }
}
