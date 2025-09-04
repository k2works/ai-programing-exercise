using System.ComponentModel.DataAnnotations;

namespace MRS.Application.DTOs;

public record ReservationDto(
    string ReservationId,
    string RoomId,
    string UserId,
    string Title,
    DateTime StartTime,
    DateTime EndTime,
    List<string> Participants,
    string Status,
    int RowVersion,
    DateTime CreatedAt,
    DateTime UpdatedAt
);

public record CreateReservationRequest(
    [Required] string RoomId,
    [Required] string UserId,
    [Required] string Title,
    [Required] DateTime StartTime,
    [Required] DateTime EndTime,
    List<string>? Participants = null
);

public record UpdateReservationRequest(
    [Required] string Title,
    [Required] DateTime StartTime,
    [Required] DateTime EndTime,
    List<string>? Participants = null
);