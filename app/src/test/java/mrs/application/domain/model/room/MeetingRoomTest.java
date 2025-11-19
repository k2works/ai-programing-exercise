package mrs.application.domain.model.room;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class MeetingRoomTest {

    @Test
    void 会議室を作成できる() {
        RoomId roomId = new RoomId(1);
        RoomName roomName = new RoomName("会議室A");

        MeetingRoom meetingRoom = new MeetingRoom(roomId, roomName);

        assertEquals(roomId, meetingRoom.roomId());
        assertEquals(roomName, meetingRoom.roomName());
    }

    @Test
    void nullのroomIdでは作成できない() {
        assertThrows(IllegalArgumentException.class, 
            () -> new MeetingRoom(null, new RoomName("会議室A")));
    }

    @Test
    void nullのroomNameでは作成できない() {
        assertThrows(IllegalArgumentException.class, 
            () -> new MeetingRoom(new RoomId(1), null));
    }
}
