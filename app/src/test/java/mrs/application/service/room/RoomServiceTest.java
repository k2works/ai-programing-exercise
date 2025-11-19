package mrs.application.service.room;

import mrs.application.domain.model.reservation.*;
import mrs.application.domain.model.room.*;
import mrs.application.port.out.MeetingRoomPort;
import mrs.application.port.out.ReservableRoomPort;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class RoomServiceTest {
    private ReservableRoomPort reservableRoomPort;
    private MeetingRoomPort meetingRoomPort;
    private RoomService service;

    @BeforeEach
    void setUp() {
        reservableRoomPort = mock(ReservableRoomPort.class);
        meetingRoomPort = mock(MeetingRoomPort.class);
        service = new RoomService(reservableRoomPort, meetingRoomPort);
    }

    @Test
    void 予約可能会議室一覧を取得できる() {
        ReservedDate date = new ReservedDate(LocalDate.of(2024, 1, 15));
        ReservableRoom room = createReservableRoom(1);
        ReservableRoomList roomList = new ReservableRoomList(List.of(room));
        
        when(reservableRoomPort.findByReservableRoomId_reservedDateOrderByReservableRoomId_roomIdAsc(date))
            .thenReturn(roomList);

        ReservableRoomList result = service.findReservableRooms(date);

        assertEquals(1, result.size());
    }

    @Test
    void 会議室を取得できる() {
        RoomId roomId = new RoomId(1);
        MeetingRoom meetingRoom = new MeetingRoom(roomId, new RoomName("会議室A"));
        
        when(meetingRoomPort.findById(roomId)).thenReturn(meetingRoom);

        MeetingRoom result = service.findMeetingRoom(roomId);

        assertNotNull(result);
        assertEquals("会議室A", result.roomName().value());
    }

    private ReservableRoom createReservableRoom(int roomId) {
        ReservableRoomId id = new ReservableRoomId(LocalDate.of(2024, 1, 15), new RoomId(roomId));
        MeetingRoom meetingRoom = new MeetingRoom(new RoomId(roomId), new RoomName("会議室" + roomId));
        return new ReservableRoom(id, meetingRoom);
    }
}
