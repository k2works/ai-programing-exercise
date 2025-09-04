namespace MRS.Application.DTOs;

public record CancelReservationRequest
{
    public string Reason { get; init; } = string.Empty;
    public string UserId { get; init; } = string.Empty;
    public bool IsAdmin { get; init; }
}