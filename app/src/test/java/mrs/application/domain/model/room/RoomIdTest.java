package mrs.application.domain.model.room;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class RoomIdTest {

    @Test
    void 会議室IDを作成できる() {
        RoomId roomId = new RoomId(1);
        assertEquals(1, roomId.value());
    }

    @Test
    void 同じ値のRoomIdは等しい() {
        RoomId roomId1 = new RoomId(1);
        RoomId roomId2 = new RoomId(1);
        assertEquals(roomId1, roomId2);
        assertEquals(roomId1.hashCode(), roomId2.hashCode());
    }
}
