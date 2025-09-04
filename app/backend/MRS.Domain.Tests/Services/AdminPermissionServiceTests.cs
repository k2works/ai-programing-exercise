using MRS.Domain.Entities;
using MRS.Domain.Services;
using MRS.Domain.ValueObjects;

namespace MRS.Domain.Tests.Services;

public class AdminPermissionServiceTests
{
    [Fact]
    public void CanCancelOthersReservations_WithAdminUser_ShouldReturnTrue()
    {
        // Arrange
        var adminUser = new User(
            new UserId("admin01"),
            new Name("管理者"),
            new Password("password123"),
            UserRole.Admin
        );
        var service = new AdminPermissionService();
        
        // Act
        var result = service.CanCancelOthersReservations(adminUser);
        
        // Assert
        Assert.True(result);
    }
    
    [Fact]
    public void CanCancelOthersReservations_WithRegularUser_ShouldReturnFalse()
    {
        // Arrange
        var regularUser = new User(
            new UserId("user01"),
            new Name("一般ユーザー"),
            new Password("password123"),
            UserRole.Member
        );
        var service = new AdminPermissionService();
        
        // Act
        var result = service.CanCancelOthersReservations(regularUser);
        
        // Assert
        Assert.False(result);
    }
    
    [Fact]
    public void CanCancelOthersReservations_WithNullUser_ShouldThrowArgumentNullException()
    {
        // Arrange
        var service = new AdminPermissionService();
        
        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => service.CanCancelOthersReservations(null!));
    }
    
    [Fact]
    public void CanModifyReservation_WithReservationOwner_ShouldReturnTrue()
    {
        // Arrange
        var userId = new UserId("user01");
        var user = new User(
            userId,
            new Name("一般ユーザー"),
            new Password("password123"),
            UserRole.Member
        );
        var reservation = CreateTestReservation(userId.Value);
        var service = new AdminPermissionService();
        
        // Act
        var result = service.CanModifyReservation(user, reservation);
        
        // Assert
        Assert.True(result);
    }
    
    [Fact]
    public void CanModifyReservation_WithAdminUser_ShouldReturnTrue()
    {
        // Arrange
        var adminUser = new User(
            new UserId("admin01"),
            new Name("管理者"),
            new Password("password123"),
            UserRole.Admin
        );
        var reservation = CreateTestReservation("other_user");
        var service = new AdminPermissionService();
        
        // Act
        var result = service.CanModifyReservation(adminUser, reservation);
        
        // Assert
        Assert.True(result);
    }
    
    [Fact]
    public void CanModifyReservation_WithDifferentUser_ShouldReturnFalse()
    {
        // Arrange
        var user = new User(
            new UserId("user01"),
            new Name("一般ユーザー"),
            new Password("password123"),
            UserRole.Member
        );
        var reservation = CreateTestReservation("other_user");
        var service = new AdminPermissionService();
        
        // Act
        var result = service.CanModifyReservation(user, reservation);
        
        // Assert
        Assert.False(result);
    }
    
    [Fact]
    public void CanModifyReservation_WithNullUser_ShouldThrowArgumentNullException()
    {
        // Arrange
        var reservation = CreateTestReservation("user01");
        var service = new AdminPermissionService();
        
        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => service.CanModifyReservation(null!, reservation));
    }
    
    [Fact]
    public void CanModifyReservation_WithNullReservation_ShouldThrowArgumentNullException()
    {
        // Arrange
        var user = new User(
            new UserId("user01"),
            new Name("一般ユーザー"),
            new Password("password123"),
            UserRole.Member
        );
        var service = new AdminPermissionService();
        
        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => service.CanModifyReservation(user, null!));
    }
    
    [Fact]
    public void ValidateAdminOperation_WithAdminUser_ShouldNotThrow()
    {
        // Arrange
        var adminUser = new User(
            new UserId("admin01"),
            new Name("管理者"),
            new Password("password123"),
            UserRole.Admin
        );
        var service = new AdminPermissionService();
        
        // Act & Assert (例外が投げられないことを確認)
        var exception = Record.Exception(() => service.ValidateAdminOperation(adminUser, "テスト操作"));
        Assert.Null(exception);
    }
    
    [Fact]
    public void ValidateAdminOperation_WithRegularUser_ShouldThrowUnauthorizedAccessException()
    {
        // Arrange
        var regularUser = new User(
            new UserId("user01"),
            new Name("一般ユーザー"),
            new Password("password123"),
            UserRole.Member
        );
        var service = new AdminPermissionService();
        
        // Act & Assert
        var exception = Assert.Throws<UnauthorizedAccessException>(
            () => service.ValidateAdminOperation(regularUser, "管理者操作"));
        
        Assert.Contains("管理者操作", exception.Message);
        Assert.Contains("管理者権限", exception.Message);
    }
    
    private static Reservation CreateTestReservation(string userId)
    {
        var timeSlot = new TimeSlot(
            DateTime.Today.AddHours(10),
            DateTime.Today.AddHours(11)
        );
        
        var reservation = Reservation.Create(
            "room01",
            userId,
            "テスト会議",
            timeSlot,
            new List<string> { "participant01" }
        );
        reservation.SetReservationId("test-reservation-01");
        return reservation;
    }
}