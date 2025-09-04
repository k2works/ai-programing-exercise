using MRS.Application.DTOs;
using MRS.Application.Ports;
using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;

namespace MRS.Application.Services;

public class ReservationService : IReservationService
{
    private readonly IReservationRepository _reservationRepository;

    public ReservationService(IReservationRepository reservationRepository)
    {
        _reservationRepository = reservationRepository ?? throw new ArgumentNullException(nameof(reservationRepository));
    }

    public async Task<ReservationDto?> GetReservationByIdAsync(string reservationId)
    {
        if (string.IsNullOrWhiteSpace(reservationId))
            throw new ArgumentException("Reservation ID cannot be null or empty.", nameof(reservationId));

        var reservation = await _reservationRepository.GetByIdAsync(reservationId);
        return reservation?.ToDto();
    }

    public async Task<IReadOnlyList<ReservationDto>> GetAllReservationsAsync()
    {
        var reservations = await _reservationRepository.GetAllAsync();
        return reservations.Select(r => r.ToDto()).ToList().AsReadOnly();
    }

    public async Task<IReadOnlyList<ReservationDto>> GetReservationsByRoomIdAsync(string roomId)
    {
        if (string.IsNullOrWhiteSpace(roomId))
            throw new ArgumentException("Room ID cannot be null or empty.", nameof(roomId));

        var reservations = await _reservationRepository.GetByRoomIdAsync(roomId);
        return reservations.Select(r => r.ToDto()).ToList().AsReadOnly();
    }

    public async Task<IReadOnlyList<ReservationDto>> GetReservationsByUserIdAsync(string userId)
    {
        if (string.IsNullOrWhiteSpace(userId))
            throw new ArgumentException("User ID cannot be null or empty.", nameof(userId));

        var reservations = await _reservationRepository.GetByUserIdAsync(userId);
        return reservations.Select(r => r.ToDto()).ToList().AsReadOnly();
    }

    public async Task<IReadOnlyList<ReservationDto>> GetReservationsByDateRangeAsync(DateTime startDate, DateTime endDate)
    {
        if (startDate > endDate)
            throw new ArgumentException("Start date cannot be after end date.");

        var reservations = await _reservationRepository.GetByDateRangeAsync(startDate, endDate);
        return reservations.Select(r => r.ToDto()).ToList().AsReadOnly();
    }

    public async Task<ReservationDto> CreateReservationAsync(CreateReservationRequest request)
    {
        if (request == null)
            throw new ArgumentNullException(nameof(request));

        // バリデーション
        if (request.StartTime >= request.EndTime)
            throw new ArgumentException("Start time must be before end time.");

        if (request.StartTime < DateTime.UtcNow)
            throw new ArgumentException("Cannot create reservation in the past.");

        // 重複チェック
        var hasConflict = await _reservationRepository.HasConflictingReservationAsync(
            request.RoomId, request.StartTime, request.EndTime);
        
        if (hasConflict)
            throw new InvalidOperationException("The selected time slot is already reserved.");

        // ドメインオブジェクト作成
        var timeSlot = new TimeSlot(request.StartTime, request.EndTime);
        var reservation = Reservation.Create(
            request.RoomId,
            request.UserId,
            request.Title,
            timeSlot,
            request.Participants ?? new List<string>()
        );

        var reservationId = await _reservationRepository.CreateAsync(reservation);
        reservation.SetReservationId(reservationId);

        return reservation.ToDto();
    }

    public async Task<ReservationDto> UpdateReservationAsync(string reservationId, UpdateReservationRequest request, int expectedRowVersion)
    {
        if (string.IsNullOrWhiteSpace(reservationId))
            throw new ArgumentException("Reservation ID cannot be null or empty.", nameof(reservationId));
        
        if (request == null)
            throw new ArgumentNullException(nameof(request));

        var existingReservation = await _reservationRepository.GetByIdAsync(reservationId);
        if (existingReservation == null)
            throw new InvalidOperationException("Reservation not found.");

        if (existingReservation.RowVersion != expectedRowVersion)
            throw new InvalidOperationException("Reservation has been modified by another user.");

        // バリデーション
        if (request.StartTime >= request.EndTime)
            throw new ArgumentException("Start time must be before end time.");

        // 重複チェック（自分以外）
        var hasConflict = await _reservationRepository.HasConflictingReservationAsync(
            existingReservation.RoomId, request.StartTime, request.EndTime, reservationId);
        
        if (hasConflict)
            throw new InvalidOperationException("The selected time slot is already reserved.");

        // 更新
        var timeSlot = new TimeSlot(request.StartTime, request.EndTime);
        existingReservation.Update(
            request.Title,
            timeSlot,
            request.Participants ?? new List<string>()
        );

        var success = await _reservationRepository.UpdateAsync(existingReservation);
        if (!success)
            throw new InvalidOperationException("Failed to update reservation.");

        return existingReservation.ToDto();
    }

    public async Task<bool> CancelReservationAsync(string reservationId)
    {
        if (string.IsNullOrWhiteSpace(reservationId))
            throw new ArgumentException("Reservation ID cannot be null or empty.", nameof(reservationId));

        return await _reservationRepository.DeleteAsync(reservationId);
    }
}

public static class ReservationExtensions
{
    public static ReservationDto ToDto(this Reservation reservation)
    {
        return new ReservationDto(
            reservation.ReservationId,
            reservation.RoomId,
            reservation.UserId,
            reservation.Title,
            reservation.TimeSlot.StartTime,
            reservation.TimeSlot.EndTime,
            reservation.Participants.ToList(),
            reservation.Status,
            reservation.RowVersion,
            reservation.CreatedAt,
            reservation.UpdatedAt
        );
    }
}