using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;

namespace MRS.Domain.Tests.Entities;

public class ReservationCancellationTests
{
    [Fact]
    public void CancelByUser_WithValidCancellationRequest_ShouldUpdateStatus()
    {
        // Arrange
        var reservation = CreateTestReservation();
        var cancellationRequest = CancellationRequest.Create("会議延期のため", DateTime.UtcNow);
        
        // Act
        reservation.CancelByUser(cancellationRequest, "user01");
        
        // Assert
        Assert.Equal(ReservationStatus.Cancelled, reservation.Status);
        Assert.NotNull(reservation.CancelledAt);
        Assert.Equal("user01", reservation.CancelledBy);
        Assert.Equal("会議延期のため", reservation.CancellationReason);
    }
    
    [Fact]
    public void CancelByUser_WithDifferentUser_ShouldThrowUnauthorizedAccessException()
    {
        // Arrange
        var reservation = CreateTestReservation();
        var cancellationRequest = CancellationRequest.Create("会議延期のため", DateTime.UtcNow);
        
        // Act & Assert
        Assert.Throws<UnauthorizedAccessException>(() => 
            reservation.CancelByUser(cancellationRequest, "different_user"));
    }
    
    [Fact]
    public void CancelByAdmin_WithValidCancellationRequest_ShouldUpdateStatus()
    {
        // Arrange
        var reservation = CreateTestReservation();
        var cancellationRequest = CancellationRequest.Create("緊急メンテナンスのため", DateTime.UtcNow);
        
        // Act
        reservation.CancelByAdmin(cancellationRequest, "admin01");
        
        // Assert
        Assert.Equal(ReservationStatus.CancelledByAdmin, reservation.Status);
        Assert.NotNull(reservation.CancelledAt);
        Assert.Equal("admin01", reservation.CancelledBy);
        Assert.Equal("緊急メンテナンスのため", reservation.CancellationReason);
    }
    
    [Fact]
    public void CancelByUser_WithAlreadyCancelledReservation_ShouldThrowInvalidOperationException()
    {
        // Arrange
        var reservation = CreateTestReservation();
        var cancellationRequest1 = CancellationRequest.Create("最初のキャンセル", DateTime.UtcNow);
        var cancellationRequest2 = CancellationRequest.Create("二度目のキャンセル", DateTime.UtcNow);
        
        reservation.CancelByUser(cancellationRequest1, "user01");
        
        // Act & Assert
        Assert.Throws<InvalidOperationException>(() =>
            reservation.CancelByUser(cancellationRequest2, "user01"));
    }
    
    [Fact]
    public void CancelByAdmin_WithAlreadyCancelledReservation_ShouldThrowInvalidOperationException()
    {
        // Arrange
        var reservation = CreateTestReservation();
        var cancellationRequest1 = CancellationRequest.Create("最初のキャンセル", DateTime.UtcNow);
        var cancellationRequest2 = CancellationRequest.Create("二度目のキャンセル", DateTime.UtcNow);
        
        reservation.CancelByAdmin(cancellationRequest1, "admin01");
        
        // Act & Assert
        Assert.Throws<InvalidOperationException>(() =>
            reservation.CancelByAdmin(cancellationRequest2, "admin01"));
    }
    
    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData("   ")]
    public void CancelByUser_WithInvalidUserId_ShouldThrowArgumentException(string invalidUserId)
    {
        // Arrange
        var reservation = CreateTestReservation();
        var cancellationRequest = CancellationRequest.Create("会議延期のため", DateTime.UtcNow);
        
        // Act & Assert
        Assert.Throws<ArgumentException>(() =>
            reservation.CancelByUser(cancellationRequest, invalidUserId));
    }
    
    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData("   ")]
    public void CancelByAdmin_WithInvalidAdminId_ShouldThrowArgumentException(string invalidAdminId)
    {
        // Arrange
        var reservation = CreateTestReservation();
        var cancellationRequest = CancellationRequest.Create("緊急メンテナンスのため", DateTime.UtcNow);
        
        // Act & Assert
        Assert.Throws<ArgumentException>(() =>
            reservation.CancelByAdmin(cancellationRequest, invalidAdminId));
    }
    
    [Fact]
    public void CancelByUser_WithNullCancellationRequest_ShouldThrowArgumentNullException()
    {
        // Arrange
        var reservation = CreateTestReservation();
        
        // Act & Assert
        Assert.Throws<ArgumentNullException>(() =>
            reservation.CancelByUser(null!, "user01"));
    }
    
    [Fact]
    public void CancelByAdmin_WithNullCancellationRequest_ShouldThrowArgumentNullException()
    {
        // Arrange
        var reservation = CreateTestReservation();
        
        // Act & Assert
        Assert.Throws<ArgumentNullException>(() =>
            reservation.CancelByAdmin(null!, "admin01"));
    }
    
    [Fact]
    public void IsCancelled_WithConfirmedReservation_ShouldReturnFalse()
    {
        // Arrange
        var reservation = CreateTestReservation();
        
        // Act & Assert
        Assert.False(reservation.IsCancelled);
    }
    
    [Fact]
    public void IsCancelled_WithCancelledReservation_ShouldReturnTrue()
    {
        // Arrange
        var reservation = CreateTestReservation();
        var cancellationRequest = CancellationRequest.Create("会議延期のため", DateTime.UtcNow);
        
        // Act
        reservation.CancelByUser(cancellationRequest, "user01");
        
        // Assert
        Assert.True(reservation.IsCancelled);
    }
    
    [Fact]
    public void IsCancelled_WithAdminCancelledReservation_ShouldReturnTrue()
    {
        // Arrange
        var reservation = CreateTestReservation();
        var cancellationRequest = CancellationRequest.Create("緊急メンテナンスのため", DateTime.UtcNow);
        
        // Act
        reservation.CancelByAdmin(cancellationRequest, "admin01");
        
        // Assert
        Assert.True(reservation.IsCancelled);
    }
    
    private static Reservation CreateTestReservation()
    {
        var timeSlot = new TimeSlot(
            DateTime.Today.AddHours(10),
            DateTime.Today.AddHours(11)
        );
        
        var reservation = Reservation.Create(
            "room01",
            "user01",
            "テスト会議",
            timeSlot,
            new List<string> { "participant01" }
        );
        
        reservation.SetReservationId("test-reservation-01");
        return reservation;
    }
}