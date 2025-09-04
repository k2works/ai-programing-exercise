using MRS.Application.DTOs;

namespace MRS.Application.Services;

public interface IReservationService
{
    Task<ReservationDto?> GetReservationByIdAsync(string reservationId);
    Task<IReadOnlyList<ReservationDto>> GetAllReservationsAsync();
    Task<IReadOnlyList<ReservationDto>> GetReservationsByRoomIdAsync(string roomId);
    Task<IReadOnlyList<ReservationDto>> GetReservationsByUserIdAsync(string userId);
    Task<IReadOnlyList<ReservationDto>> GetReservationsByDateRangeAsync(DateTime startDate, DateTime endDate);
    Task<ReservationDto> CreateReservationAsync(CreateReservationRequest request);
    Task<ReservationDto> UpdateReservationAsync(string reservationId, UpdateReservationRequest request, int expectedRowVersion);
    Task<bool> CancelReservationAsync(string reservationId);
    Task<bool> CancelReservationWithDetailsAsync(string reservationId, CancelReservationRequest request);
}