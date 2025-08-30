package mrs.application.port.in;

import mrs.application.domain.model.reservation.Reservation;
import mrs.application.domain.model.auth.User;
import java.time.LocalDate;
import java.util.List;

public interface ReservationUseCase {
    List<Reservation> findReservations(Integer roomId, LocalDate date);
    Reservation reserve(Reservation reservation, User user);
    void cancel(Integer reservationId, User user);
}