using MRS.Domain.ValueObjects;

namespace MRS.Domain.Tests.ValueObjects;

public class TimeSlotTests
{
    [Fact]
    public void Constructor_WithValidTimes_CreatesTimeSlot()
    {
        // Arrange
        var startTime = DateTime.UtcNow;
        var endTime = startTime.AddHours(1);

        // Act
        var timeSlot = new TimeSlot(startTime, endTime);

        // Assert
        Assert.Equal(startTime, timeSlot.StartTime);
        Assert.Equal(endTime, timeSlot.EndTime);
    }

    [Fact]
    public void Constructor_WithStartTimeAfterEndTime_ThrowsArgumentException()
    {
        // Arrange
        var startTime = DateTime.UtcNow.AddHours(2);
        var endTime = DateTime.UtcNow.AddHours(1);

        // Act & Assert
        Assert.Throws<ArgumentException>(() => new TimeSlot(startTime, endTime));
    }

    [Fact]
    public void Constructor_WithStartTimeEqualToEndTime_ThrowsArgumentException()
    {
        // Arrange
        var time = DateTime.UtcNow;

        // Act & Assert
        Assert.Throws<ArgumentException>(() => new TimeSlot(time, time));
    }

    [Theory]
    [InlineData("2023-01-01 10:00", "2023-01-01 12:00", "2023-01-01 11:00", "2023-01-01 13:00", true)] // Overlapping - partial overlap at end
    [InlineData("2023-01-01 10:00", "2023-01-01 12:00", "2023-01-01 12:00", "2023-01-01 14:00", false)] // Adjacent (no overlap)
    [InlineData("2023-01-01 10:00", "2023-01-01 12:00", "2023-01-01 08:00", "2023-01-01 10:00", false)] // Adjacent (no overlap)
    [InlineData("2023-01-01 10:00", "2023-01-01 12:00", "2023-01-01 09:00", "2023-01-01 11:00", true)] // Overlapping - partial overlap at start
    [InlineData("2023-01-01 10:00", "2023-01-01 12:00", "2023-01-01 13:00", "2023-01-01 15:00", false)] // No overlap - completely after
    [InlineData("2023-01-01 10:00", "2023-01-01 12:00", "2023-01-01 08:00", "2023-01-01 14:00", true)] // Completely containing
    [InlineData("2023-01-01 10:00", "2023-01-01 12:00", "2023-01-01 10:30", "2023-01-01 11:30", true)] // Completely contained
    public void OverlapsWith_WithVariousTimeSlots_ReturnsExpectedResult(
        string startTime1, string endTime1, string startTime2, string endTime2, bool expected)
    {
        // Arrange
        var timeSlot1 = new TimeSlot(DateTime.Parse(startTime1), DateTime.Parse(endTime1));
        var timeSlot2 = new TimeSlot(DateTime.Parse(startTime2), DateTime.Parse(endTime2));

        // Act
        var result = timeSlot1.OverlapsWith(timeSlot2);

        // Assert
        Assert.Equal(expected, result);
    }

    [Fact]
    public void OverlapsWith_WithNullTimeSlot_ThrowsArgumentNullException()
    {
        // Arrange
        var timeSlot = new TimeSlot(DateTime.UtcNow, DateTime.UtcNow.AddHours(1));

        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => timeSlot.OverlapsWith(null!));
    }

    [Fact]
    public void Duration_ReturnsCorrectTimeSpan()
    {
        // Arrange
        var startTime = DateTime.UtcNow;
        var endTime = startTime.AddHours(2).AddMinutes(30);
        var timeSlot = new TimeSlot(startTime, endTime);
        var expectedDuration = TimeSpan.FromHours(2.5);

        // Act
        var duration = timeSlot.Duration;

        // Assert
        Assert.Equal(expectedDuration, duration);
    }

    [Theory]
    [InlineData("2023-01-01 10:00", "2023-01-01 12:00", "2023-01-01 10:30", true)]  // Inside
    [InlineData("2023-01-01 10:00", "2023-01-01 12:00", "2023-01-01 10:00", true)]  // At start
    [InlineData("2023-01-01 10:00", "2023-01-01 12:00", "2023-01-01 12:00", false)] // At end (exclusive)
    [InlineData("2023-01-01 10:00", "2023-01-01 12:00", "2023-01-01 09:59", false)] // Before start
    [InlineData("2023-01-01 10:00", "2023-01-01 12:00", "2023-01-01 12:01", false)] // After end
    public void Contains_WithVariousTimes_ReturnsExpectedResult(
        string startTime, string endTime, string testTime, bool expected)
    {
        // Arrange
        var timeSlot = new TimeSlot(DateTime.Parse(startTime), DateTime.Parse(endTime));
        var dateTime = DateTime.Parse(testTime);

        // Act
        var result = timeSlot.Contains(dateTime);

        // Assert
        Assert.Equal(expected, result);
    }

    [Fact]
    public void ToString_ReturnsFormattedString()
    {
        // Arrange
        var startTime = new DateTime(2023, 1, 1, 10, 30, 0);
        var endTime = new DateTime(2023, 1, 1, 12, 45, 0);
        var timeSlot = new TimeSlot(startTime, endTime);

        // Act
        var result = timeSlot.ToString();

        // Assert
        Assert.Equal("2023-01-01 10:30 - 2023-01-01 12:45", result);
    }

    [Fact]
    public void Equals_WithSameTimeSlots_ReturnsTrue()
    {
        // Arrange
        var startTime = DateTime.UtcNow;
        var endTime = startTime.AddHours(1);
        var timeSlot1 = new TimeSlot(startTime, endTime);
        var timeSlot2 = new TimeSlot(startTime, endTime);

        // Act & Assert
        Assert.Equal(timeSlot1, timeSlot2);
        Assert.True(timeSlot1 == timeSlot2);
    }

    [Fact]
    public void Equals_WithDifferentTimeSlots_ReturnsFalse()
    {
        // Arrange
        var startTime = DateTime.UtcNow;
        var timeSlot1 = new TimeSlot(startTime, startTime.AddHours(1));
        var timeSlot2 = new TimeSlot(startTime, startTime.AddHours(2));

        // Act & Assert
        Assert.NotEqual(timeSlot1, timeSlot2);
        Assert.True(timeSlot1 != timeSlot2);
    }

    [Fact]
    public void GetHashCode_WithSameTimeSlots_ReturnsSameHashCode()
    {
        // Arrange
        var startTime = DateTime.UtcNow;
        var endTime = startTime.AddHours(1);
        var timeSlot1 = new TimeSlot(startTime, endTime);
        var timeSlot2 = new TimeSlot(startTime, endTime);

        // Act & Assert
        Assert.Equal(timeSlot1.GetHashCode(), timeSlot2.GetHashCode());
    }
}