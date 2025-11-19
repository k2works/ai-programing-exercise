package mrs.application.domain.model.room;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class RoomNameTest {

    @Test
    void 会議室名を作成できる() {
        RoomName roomName = new RoomName("会議室A");
        assertEquals("会議室A", roomName.value());
    }

    @Test
    void nullの会議室名は作成できない() {
        assertThrows(IllegalArgumentException.class, () -> new RoomName(null));
    }

    @Test
    void 空文字の会議室名は作成できない() {
        assertThrows(IllegalArgumentException.class, () -> new RoomName(""));
    }

    @Test
    void 同じ値のRoomNameは等しい() {
        RoomName roomName1 = new RoomName("会議室A");
        RoomName roomName2 = new RoomName("会議室A");
        assertEquals(roomName1, roomName2);
        assertEquals(roomName1.hashCode(), roomName2.hashCode());
    }
}
