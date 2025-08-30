package mrs.port.in;

import mrs.domain.model.reservation.Reservation;
import mrs.domain.model.auth.User;
import java.time.LocalDate;
import java.util.List;

public interface ReservationUseCase {
    List<Reservation> findReservations(Integer roomId, LocalDate date);
    Reservation reserve(Reservation reservation, User user);
    void cancel(Integer reservationId, User user);
}