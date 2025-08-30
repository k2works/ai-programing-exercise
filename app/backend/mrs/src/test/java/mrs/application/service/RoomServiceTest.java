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
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class RoomServiceTest {

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
        verify(meetingRoomPort, times(1)).findAll();
    }

    @Test
    void testFindAllMeetingRooms_空のリスト() {
        // Arrange
        List<MeetingRoom> emptyRooms = Collections.emptyList();
        when(meetingRoomPort.findAll()).thenReturn(emptyRooms);

        // Act
        List<MeetingRoom> actualRooms = roomService.findAllMeetingRooms();

        // Assert
        assertTrue(actualRooms.isEmpty());
        verify(meetingRoomPort, times(1)).findAll();
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
        verify(reservableRoomPort, times(1)).findByReservableDate(date);
    }

    @Test
    void testFindReservableRooms_空のリスト() {
        // Arrange
        LocalDate date = LocalDate.of(2025, 1, 1);
        List<ReservableRoom> emptyRooms = Collections.emptyList();
        when(reservableRoomPort.findByReservableDate(date)).thenReturn(emptyRooms);

        // Act
        List<ReservableRoom> actualRooms = roomService.findReservableRooms(date);

        // Assert
        assertTrue(actualRooms.isEmpty());
        verify(reservableRoomPort, times(1)).findByReservableDate(date);
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
        verify(meetingRoomPort, times(1)).findById(roomId);
    }

    @Test
    void testFindMeetingRoom_存在しない場合() {
        // Arrange
        Integer roomId = 999;
        when(meetingRoomPort.findById(roomId)).thenReturn(null);

        // Act
        MeetingRoom actualRoom = roomService.findMeetingRoom(roomId);

        // Assert
        assertNull(actualRoom);
        verify(meetingRoomPort, times(1)).findById(roomId);
    }

    @Test
    void testFindReservableRooms_複数の部屋() {
        // Arrange
        LocalDate date = LocalDate.of(2025, 1, 1);
        
        ReservableRoom room1 = new ReservableRoom();
        room1.setRoomId(1);
        room1.setReservableDate(date);
        
        ReservableRoom room2 = new ReservableRoom();
        room2.setRoomId(2);
        room2.setReservableDate(date);
        
        List<ReservableRoom> expectedRooms = Arrays.asList(room1, room2);
        when(reservableRoomPort.findByReservableDate(date)).thenReturn(expectedRooms);

        // Act
        List<ReservableRoom> actualRooms = roomService.findReservableRooms(date);

        // Assert
        assertEquals(2, actualRooms.size());
        assertEquals(expectedRooms, actualRooms);
        verify(reservableRoomPort, times(1)).findByReservableDate(date);
    }

    @Test
    void testFindAllMeetingRooms_複数の部屋() {
        // Arrange
        MeetingRoom room1 = new MeetingRoom();
        room1.setRoomId(1);
        room1.setRoomName("会議室A");
        
        MeetingRoom room2 = new MeetingRoom();
        room2.setRoomId(2);
        room2.setRoomName("会議室B");
        
        List<MeetingRoom> expectedRooms = Arrays.asList(room1, room2);
        when(meetingRoomPort.findAll()).thenReturn(expectedRooms);

        // Act
        List<MeetingRoom> actualRooms = roomService.findAllMeetingRooms();

        // Assert
        assertEquals(2, actualRooms.size());
        assertEquals(expectedRooms, actualRooms);
        verify(meetingRoomPort, times(1)).findAll();
    }
}