using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;

namespace MRS.Domain.Services;

public class ReservationDomainService
{
    public bool CanReserve(Reservation newReservation, IEnumerable<Reservation> existingReservations)
    {
        return !GetConflictingReservations(newReservation, existingReservations).Any();
    }

    public IEnumerable<Reservation> GetConflictingReservations(Reservation newReservation, IEnumerable<Reservation> existingReservations)
    {
        return existingReservations.Where(existing =>
        {
            // 同じ部屋のみチェック
            if (existing.RoomId != newReservation.RoomId)
                return false;
            
            // キャンセル済み予約は除外
            if (existing.IsCancelled)
                return false;
            
            // 時間帯の重複をチェック
            return newReservation.TimeSlot.OverlapsWith(existing.TimeSlot);
        });
    }
}