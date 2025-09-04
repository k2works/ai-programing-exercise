using MRS.Domain.ValueObjects;

namespace MRS.Domain.Tests.ValueObjects;

public class CancellationRequestTests
{
    [Fact]
    public void Create_WithValidReasonAndTimestamp_ShouldCreateCancellationRequest()
    {
        // Arrange
        var reason = "会議延期のため";
        var timestamp = DateTime.UtcNow;
        
        // Act
        var cancellationRequest = CancellationRequest.Create(reason, timestamp);
        
        // Assert
        Assert.NotNull(cancellationRequest);
        Assert.Equal(reason, cancellationRequest.Reason);
        Assert.Equal(timestamp, cancellationRequest.RequestedAt);
    }
    
    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData("   ")]
    public void Create_WithInvalidReason_ShouldThrowArgumentException(string invalidReason)
    {
        // Arrange
        var timestamp = DateTime.UtcNow;
        
        // Act & Assert
        Assert.Throws<ArgumentException>(() => CancellationRequest.Create(invalidReason, timestamp));
    }
    
    [Fact]
    public void Create_WithReasonTooLong_ShouldThrowArgumentException()
    {
        // Arrange
        var longReason = new string('あ', 501); // 500文字を超える理由
        var timestamp = DateTime.UtcNow;
        
        // Act & Assert
        Assert.Throws<ArgumentException>(() => CancellationRequest.Create(longReason, timestamp));
    }
    
    [Fact]
    public void Create_WithFutureTimestamp_ShouldThrowArgumentException()
    {
        // Arrange
        var reason = "会議延期のため";
        var futureTimestamp = DateTime.UtcNow.AddHours(1);
        
        // Act & Assert
        Assert.Throws<ArgumentException>(() => CancellationRequest.Create(reason, futureTimestamp));
    }
    
    [Fact]
    public void Create_WithCurrentTime_ShouldSucceed()
    {
        // Arrange
        var reason = "会議延期のため";
        var now = DateTime.UtcNow;
        
        // Act
        var cancellationRequest = CancellationRequest.Create(reason, now);
        
        // Assert
        Assert.NotNull(cancellationRequest);
        Assert.Equal(reason, cancellationRequest.Reason);
        Assert.Equal(now, cancellationRequest.RequestedAt);
    }
    
    [Fact]
    public void Equality_WithSameReasonAndTimestamp_ShouldBeEqual()
    {
        // Arrange
        var reason = "会議延期のため";
        var timestamp = DateTime.UtcNow;
        var request1 = CancellationRequest.Create(reason, timestamp);
        var request2 = CancellationRequest.Create(reason, timestamp);
        
        // Act & Assert
        Assert.Equal(request1, request2);
        Assert.True(request1 == request2);
        Assert.False(request1 != request2);
        Assert.Equal(request1.GetHashCode(), request2.GetHashCode());
    }
    
    [Fact]
    public void Equality_WithDifferentReason_ShouldNotBeEqual()
    {
        // Arrange
        var timestamp = DateTime.UtcNow;
        var request1 = CancellationRequest.Create("理由1", timestamp);
        var request2 = CancellationRequest.Create("理由2", timestamp);
        
        // Act & Assert
        Assert.NotEqual(request1, request2);
        Assert.False(request1 == request2);
        Assert.True(request1 != request2);
    }
    
    [Fact]
    public void ToString_ShouldReturnFormattedString()
    {
        // Arrange
        var reason = "会議延期のため";
        var timestamp = DateTime.UtcNow.AddMinutes(-10); // 過去のタイムスタンプを使用
        var request = CancellationRequest.Create(reason, timestamp);
        
        // Act
        var result = request.ToString();
        
        // Assert
        Assert.Contains(reason, result);
        Assert.Contains("CancellationRequest:", result);
        Assert.Contains("Requested at:", result);
        Assert.Contains("UTC", result);
    }
}