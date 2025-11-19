package mrs.application.port.out;

import mrs.application.domain.model.reservation.*;
import java.util.List;

public interface ReservationPort {
    List<Reservation> findByReservableRoom_ReservableRoomIdOrderByStartTimeAsc(ReservableRoomId reservableRoomId);
    void save(Reservation reservation);
    void delete(Reservation reservation);
    Reservation findById(Integer id);
}
