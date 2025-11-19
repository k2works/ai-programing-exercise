package mrs.application.domain.model.reservation;

import mrs.application.domain.model.auth.UserId;

public record Reservation(
    ReservationId reservationId,
    ReservationTimeSlot timeSlot,
    ReservableRoomId reservableRoomId,
    UserId userId
) {
    public boolean overlap(Reservation other) {
        if (!this.reservableRoomId.equals(other.reservableRoomId)) {
            return false;
        }
        return this.timeSlot.overlap(other.timeSlot);
    }
}
