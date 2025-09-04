using MRS.Domain.ValueObjects;

namespace MRS.Domain.Entities;

public class Reservation
{
    public string ReservationId { get; private set; } = string.Empty;
    public string RoomId { get; private set; } = string.Empty;
    public string UserId { get; private set; } = string.Empty;
    public string Title { get; private set; } = string.Empty;
    public TimeSlot TimeSlot { get; private set; } = null!;
    public IReadOnlyList<string> Participants { get; private set; } = new List<string>();
    public ReservationStatus Status { get; private set; } = ReservationStatus.Confirmed;
    public int RowVersion { get; private set; }
    public DateTime CreatedAt { get; private set; }
    public DateTime UpdatedAt { get; private set; }
    
    public DateTime? CancelledAt { get; private set; }
    public string? CancelledBy { get; private set; }
    public string? CancellationReason { get; private set; }
    
    public bool IsCancelled => Status == ReservationStatus.Cancelled || Status == ReservationStatus.CancelledByAdmin;

    private Reservation() { }

    public static Reservation Create(
        string roomId,
        string userId,
        string title,
        TimeSlot timeSlot,
        List<string> participants)
    {
        if (string.IsNullOrWhiteSpace(roomId))
            throw new ArgumentException("Room ID cannot be null or empty.", nameof(roomId));
        
        if (string.IsNullOrWhiteSpace(userId))
            throw new ArgumentException("User ID cannot be null or empty.", nameof(userId));
        
        if (string.IsNullOrWhiteSpace(title))
            throw new ArgumentException("Title cannot be null or empty.", nameof(title));
        
        if (timeSlot == null)
            throw new ArgumentNullException(nameof(timeSlot));

        var now = DateTime.UtcNow;
        return new Reservation
        {
            RoomId = roomId,
            UserId = userId,
            Title = title,
            TimeSlot = timeSlot,
            Participants = participants?.ToList().AsReadOnly() ?? new List<string>().AsReadOnly(),
            Status = ReservationStatus.Confirmed,
            RowVersion = 0,
            CreatedAt = now,
            UpdatedAt = now
        };
    }

    public void SetReservationId(string reservationId)
    {
        if (string.IsNullOrWhiteSpace(reservationId))
            throw new ArgumentException("Reservation ID cannot be null or empty.", nameof(reservationId));
        
        ReservationId = reservationId;
    }

    public void Update(string title, TimeSlot timeSlot, List<string> participants)
    {
        if (string.IsNullOrWhiteSpace(title))
            throw new ArgumentException("Title cannot be null or empty.", nameof(title));
        
        if (timeSlot == null)
            throw new ArgumentNullException(nameof(timeSlot));

        Title = title;
        TimeSlot = timeSlot;
        Participants = participants?.ToList().AsReadOnly() ?? new List<string>().AsReadOnly();
        UpdatedAt = DateTime.UtcNow;
        RowVersion++;
    }

    public void CancelByUser(CancellationRequest cancellationRequest, string userId)
    {
        ArgumentNullException.ThrowIfNull(cancellationRequest);
        
        if (string.IsNullOrWhiteSpace(userId))
            throw new ArgumentException("User ID cannot be null or empty.", nameof(userId));
            
        if (IsCancelled)
            throw new InvalidOperationException("The reservation is already cancelled.");
            
        if (UserId != userId)
            throw new UnauthorizedAccessException("Only the reservation owner can cancel their reservation.");
            
        Status = ReservationStatus.Cancelled;
        CancelledAt = DateTime.UtcNow;
        CancelledBy = userId;
        CancellationReason = cancellationRequest.Reason;
        UpdatedAt = DateTime.UtcNow;
        RowVersion++;
    }
    
    public void CancelByAdmin(CancellationRequest cancellationRequest, string adminId)
    {
        ArgumentNullException.ThrowIfNull(cancellationRequest);
        
        if (string.IsNullOrWhiteSpace(adminId))
            throw new ArgumentException("Admin ID cannot be null or empty.", nameof(adminId));
            
        if (IsCancelled)
            throw new InvalidOperationException("The reservation is already cancelled.");
            
        Status = ReservationStatus.CancelledByAdmin;
        CancelledAt = DateTime.UtcNow;
        CancelledBy = adminId;
        CancellationReason = cancellationRequest.Reason;
        UpdatedAt = DateTime.UtcNow;
        RowVersion++;
    }

    public void Cancel()
    {
        Status = ReservationStatus.Cancelled;
        UpdatedAt = DateTime.UtcNow;
        RowVersion++;
    }

    public bool IsConflictWith(TimeSlot otherTimeSlot)
    {
        if (IsCancelled)
            return false;

        return TimeSlot.OverlapsWith(otherTimeSlot);
    }

    // リポジトリが使用するためのpublicメソッド
    public static Reservation Restore(
        string reservationId,
        string roomId,
        string userId,
        string title,
        TimeSlot timeSlot,
        List<string> participants,
        string status,
        int rowVersion,
        DateTime createdAt,
        DateTime updatedAt)
    {
        return new Reservation
        {
            ReservationId = reservationId,
            RoomId = roomId,
            UserId = userId,
            Title = title,
            TimeSlot = timeSlot,
            Participants = participants.AsReadOnly(),
            Status = status.ToLowerInvariant() switch
            {
                "confirmed" => ReservationStatus.Confirmed,
                "cancelled" => ReservationStatus.Cancelled,
                "cancelledbyadmin" => ReservationStatus.CancelledByAdmin,
                _ => Enum.Parse<ReservationStatus>(status, true)
            },
            RowVersion = rowVersion,
            CreatedAt = createdAt,
            UpdatedAt = updatedAt
        };
    }

    public void UpdateRowVersion(int newRowVersion)
    {
        RowVersion = newRowVersion;
    }
}