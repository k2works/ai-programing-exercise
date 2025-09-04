using MRS.Domain.Entities;

namespace MRS.Application.Ports;

public interface IReservationRepository
{
    Task<Reservation?> GetByIdAsync(string reservationId);
    Task<IReadOnlyList<Reservation>> GetAllAsync();
    Task<IReadOnlyList<Reservation>> GetByRoomIdAsync(string roomId);
    Task<IReadOnlyList<Reservation>> GetByUserIdAsync(string userId);
    Task<IReadOnlyList<Reservation>> GetByDateRangeAsync(DateTime startDate, DateTime endDate);
    Task<bool> HasConflictingReservationAsync(string roomId, DateTime startTime, DateTime endTime, string? excludeReservationId = null);
    Task<string> CreateAsync(Reservation reservation);
    Task<bool> UpdateAsync(Reservation reservation);
    Task<bool> DeleteAsync(string reservationId);
}