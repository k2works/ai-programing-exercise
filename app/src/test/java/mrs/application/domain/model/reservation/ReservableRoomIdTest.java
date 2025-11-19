package mrs.application.domain.model.reservation;

import mrs.application.domain.model.room.RoomId;
import org.junit.jupiter.api.Test;
import java.time.LocalDate;

import static org.junit.jupiter.api.Assertions.*;

class ReservableRoomIdTest {

    @Test
    void 予約可能会議室IDを作成できる() {
        LocalDate date = LocalDate.of(2024, 1, 15);
        RoomId roomId = new RoomId(1);
        ReservableRoomId id = new ReservableRoomId(date, roomId);
        assertEquals(date, id.reservedDate());
        assertEquals(roomId, id.roomId());
    }

    @Test
    void nullの日付では作成できない() {
        assertThrows(IllegalArgumentException.class, 
            () -> new ReservableRoomId(null, new RoomId(1)));
    }

    @Test
    void nullの会議室IDでは作成できない() {
        assertThrows(IllegalArgumentException.class, 
            () -> new ReservableRoomId(LocalDate.of(2024, 1, 15), null));
    }
}
