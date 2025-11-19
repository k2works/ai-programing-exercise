package mrs.application.port.in;

import mrs.application.domain.model.reservation.*;

public interface ReservationUseCase {
    ReservationList findReservations(ReservableRoomId reservableRoomId);
    Reservation reserve(Reservation reservation);
    void cancel(Reservation reservation);
    Reservation findOne(ReservationId reservationId);
}
