package mrs.application.service;

import mrs.application.domain.model.room.MeetingRoom;
import mrs.application.domain.model.room.ReservableRoom;
import mrs.application.port.out.MeetingRoomPort;
import mrs.application.port.out.ReservableRoomPort;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class RoomServiceTest {
    
    static {
        // Disable Spring Boot during unit tests
        System.setProperty("spring.main.web-application-type", "none");
        System.setProperty("spring.autoconfigure.exclude", "org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration");
    }

    @Mock
    private ReservableRoomPort reservableRoomPort;

    @Mock
    private MeetingRoomPort meetingRoomPort;

    @InjectMocks
    private RoomService roomService;

    private MeetingRoom testMeetingRoom;
    private ReservableRoom testReservableRoom;

    @BeforeEach
    void setUp() {
        // モックをリセット
        reset(reservableRoomPort, meetingRoomPort);
        
        testMeetingRoom = new MeetingRoom();
        testMeetingRoom.setRoomId(1);
        testMeetingRoom.setRoomName("会議室A");

        testReservableRoom = new ReservableRoom();
        testReservableRoom.setRoomId(1);
        testReservableRoom.setReservableDate(LocalDate.of(2025, 1, 1));
    }

    @Test
    void testFindAllMeetingRooms() {
        // Arrange
        List<MeetingRoom> expectedRooms = Arrays.asList(testMeetingRoom);
        when(meetingRoomPort.findAll()).thenReturn(expectedRooms);

        // Act
        List<MeetingRoom> actualRooms = roomService.findAllMeetingRooms();

        // Assert
        assertEquals(expectedRooms, actualRooms);
    }

    @Test
    void testFindReservableRooms() {
        // Arrange
        LocalDate date = LocalDate.of(2025, 1, 1);
        List<ReservableRoom> expectedRooms = Arrays.asList(testReservableRoom);
        when(reservableRoomPort.findByReservableDate(date)).thenReturn(expectedRooms);

        // Act
        List<ReservableRoom> actualRooms = roomService.findReservableRooms(date);

        // Assert
        assertEquals(expectedRooms, actualRooms);
    }

    @Test
    void testFindMeetingRoom() {
        // Arrange
        Integer roomId = 1;
        when(meetingRoomPort.findById(roomId)).thenReturn(testMeetingRoom);

        // Act
        MeetingRoom actualRoom = roomService.findMeetingRoom(roomId);

        // Assert
        assertEquals(testMeetingRoom, actualRoom);
    }
}