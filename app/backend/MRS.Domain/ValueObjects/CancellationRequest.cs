namespace MRS.Domain.ValueObjects;

public sealed record CancellationRequest
{
    private const int MaxReasonLength = 500;
    
    public string Reason { get; }
    public DateTime RequestedAt { get; }
    
    private CancellationRequest(string reason, DateTime requestedAt)
    {
        Reason = reason;
        RequestedAt = requestedAt;
    }
    
    public static CancellationRequest Create(string reason, DateTime requestedAt)
    {
        if (string.IsNullOrWhiteSpace(reason))
            throw new ArgumentException("Reason cannot be null, empty, or whitespace.", nameof(reason));
        
        if (reason.Length > MaxReasonLength)
            throw new ArgumentException($"Reason cannot exceed {MaxReasonLength} characters.", nameof(reason));
        
        if (requestedAt > DateTime.UtcNow)
            throw new ArgumentException("Requested timestamp cannot be in the future.", nameof(requestedAt));
        
        return new CancellationRequest(reason.Trim(), requestedAt);
    }
    
    public override string ToString()
    {
        return $"CancellationRequest: {Reason} (Requested at: {RequestedAt:yyyy-MM-dd HH:mm:ss} UTC)";
    }
}