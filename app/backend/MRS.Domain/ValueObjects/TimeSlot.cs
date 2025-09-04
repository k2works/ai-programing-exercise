namespace MRS.Domain.ValueObjects;

public record TimeSlot
{
    public DateTime StartTime { get; }
    public DateTime EndTime { get; }

    public TimeSlot(DateTime startTime, DateTime endTime)
    {
        if (startTime >= endTime)
            throw new ArgumentException("Start time must be before end time.");

        StartTime = startTime;
        EndTime = endTime;
    }

    public bool OverlapsWith(TimeSlot other)
    {
        if (other == null)
            throw new ArgumentNullException(nameof(other));

        return StartTime < other.EndTime && EndTime > other.StartTime;
    }

    public TimeSpan Duration => EndTime - StartTime;

    public bool Contains(DateTime dateTime)
    {
        return StartTime <= dateTime && dateTime < EndTime;
    }

    public override string ToString()
    {
        return $"{StartTime:yyyy-MM-dd HH:mm} - {EndTime:yyyy-MM-dd HH:mm}";
    }
}