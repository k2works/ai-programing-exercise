package mrs.application.domain.model.reservation;

import mrs.application.domain.model.room.MeetingRoom;
import mrs.application.domain.model.room.RoomId;
import mrs.application.domain.model.room.RoomName;
import org.junit.jupiter.api.Test;
import java.time.LocalDate;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class ReservableRoomListTest {

    @Test
    void 予約可能会議室リストを作成できる() {
        ReservableRoom room = createReservableRoom(1);
        ReservableRoomList list = new ReservableRoomList(List.of(room));
        assertEquals(1, list.size());
    }

    @Test
    void 空の予約可能会議室リストを作成できる() {
        ReservableRoomList list = new ReservableRoomList(List.of());
        assertEquals(0, list.size());
    }

    @Test
    void 予約可能会議室リストから要素を取得できる() {
        ReservableRoom room = createReservableRoom(1);
        ReservableRoomList list = new ReservableRoomList(List.of(room));
        assertEquals(room, list.get(0));
    }

    private ReservableRoom createReservableRoom(int roomId) {
        ReservableRoomId id = new ReservableRoomId(LocalDate.of(2024, 1, 15), new RoomId(roomId));
        MeetingRoom meetingRoom = new MeetingRoom(new RoomId(roomId), new RoomName("会議室" + roomId));
        return new ReservableRoom(id, meetingRoom);
    }
}
