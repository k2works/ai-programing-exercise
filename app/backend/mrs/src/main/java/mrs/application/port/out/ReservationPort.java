package mrs.application.port.out;

import mrs.application.domain.model.reservation.Reservation;
import java.time.LocalDate;
import java.util.List;

public interface ReservationPort {
    List<Reservation> findByRoomIdAndDate(Integer roomId, LocalDate date);
    List<Reservation> findByReservableRoomOrderByStartTimeAsc(Integer roomId, LocalDate date);
    Reservation findById(Integer reservationId);
    Reservation save(Reservation reservation);
    void delete(Integer reservationId);
}