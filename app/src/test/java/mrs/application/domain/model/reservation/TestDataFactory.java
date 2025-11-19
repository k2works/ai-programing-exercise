package mrs.application.domain.model.reservation;

import mrs.application.domain.model.room.RoomId;
import java.time.LocalDate;

public class TestDataFactory {
    public static ReservableRoomId createReservableRoomId() {
        return new ReservableRoomId(LocalDate.of(2024, 1, 15), new RoomId(1));
    }

    public static ReservableRoomId createReservableRoomId(int roomIdValue) {
        return new ReservableRoomId(LocalDate.of(2024, 1, 15), new RoomId(roomIdValue));
    }
}
