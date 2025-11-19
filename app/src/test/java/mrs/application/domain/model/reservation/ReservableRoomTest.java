package mrs.application.domain.model.reservation;

import mrs.application.domain.model.room.MeetingRoom;
import mrs.application.domain.model.room.RoomId;
import mrs.application.domain.model.room.RoomName;
import org.junit.jupiter.api.Test;
import java.time.LocalDate;

import static org.junit.jupiter.api.Assertions.*;

class ReservableRoomTest {

    @Test
    void 予約可能会議室を作成できる() {
        ReservableRoomId id = new ReservableRoomId(LocalDate.of(2024, 1, 15), new RoomId(1));
        MeetingRoom meetingRoom = new MeetingRoom(new RoomId(1), new RoomName("会議室A"));
        
        ReservableRoom room = new ReservableRoom(id, meetingRoom);
        
        assertEquals(id, room.reservableRoomId());
        assertEquals(meetingRoom, room.meetingRoom());
    }

    @Test
    void nullのIDでは作成できない() {
        MeetingRoom meetingRoom = new MeetingRoom(new RoomId(1), new RoomName("会議室A"));
        assertThrows(IllegalArgumentException.class, 
            () -> new ReservableRoom(null, meetingRoom));
    }

    @Test
    void nullの会議室では作成できない() {
        ReservableRoomId id = new ReservableRoomId(LocalDate.of(2024, 1, 15), new RoomId(1));
        assertThrows(IllegalArgumentException.class, 
            () -> new ReservableRoom(id, null));
    }
}
