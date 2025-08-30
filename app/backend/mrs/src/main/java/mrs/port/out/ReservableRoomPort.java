package mrs.port.out;

import mrs.domain.model.room.ReservableRoom;
import java.time.LocalDate;
import java.util.List;

public interface ReservableRoomPort {
    List<ReservableRoom> findByReservableDate(LocalDate date);
    ReservableRoom findOneForUpdateByReservableRoomId(Integer roomId, LocalDate date);
}