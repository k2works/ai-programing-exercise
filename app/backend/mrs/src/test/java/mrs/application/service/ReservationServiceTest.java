package mrs.application.service;

import mrs.application.domain.model.auth.User;
import mrs.application.domain.model.reservation.Reservation;
import mrs.application.domain.model.room.MeetingRoom;
import mrs.application.domain.model.room.ReservableRoom;
import mrs.application.port.out.ReservableRoomPort;
import mrs.application.port.out.ReservationPort;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class ReservationServiceTest {

    @Mock
    private ReservationPort reservationPort;

    @Mock
    private ReservableRoomPort reservableRoomPort;

    @InjectMocks
    private ReservationService reservationService;

    private User testUser;
    private ReservableRoom testReservableRoom;
    private Reservation testReservation;

    @BeforeEach
    void setUp() {
        testUser = new User();
        testUser.setUserId("user001");
        testUser.setName("田中太郎");
        testUser.setPasswordHash("hashedPassword");
        testUser.setRole("USER");

        testReservableRoom = new ReservableRoom();
        testReservableRoom.setRoomId(1);
        testReservableRoom.setReservableDate(LocalDate.of(2025, 1, 1));

        testReservation = new Reservation();
        testReservation.setReservationId(100);
        testReservation.setStartTime(LocalTime.of(10, 0));
        testReservation.setEndTime(LocalTime.of(12, 0));
        testReservation.setReservableRoom(testReservableRoom);
        testReservation.setUser(testUser);
    }

    @Test
    void testFindReservations() {
        // Arrange
        Integer roomId = 1;
        LocalDate date = LocalDate.of(2025, 1, 1);
        List<Reservation> expectedReservations = Arrays.asList(testReservation);
        
        when(reservationPort.findByRoomIdAndDate(roomId, date)).thenReturn(expectedReservations);

        // Act
        List<Reservation> actualReservations = reservationService.findReservations(roomId, date);

        // Assert
        assertEquals(expectedReservations, actualReservations);
        verify(reservationPort, times(1)).findByRoomIdAndDate(roomId, date);
    }

    @Test
    void testReserve_成功() {
        // Arrange
        when(reservableRoomPort.findOneForUpdateByReservableRoomId(anyInt(), any(LocalDate.class)))
                .thenReturn(testReservableRoom);
        when(reservationPort.findByReservableRoomOrderByStartTimeAsc(anyInt(), any(LocalDate.class)))
                .thenReturn(Collections.emptyList());
        when(reservationPort.save(any(Reservation.class))).thenReturn(testReservation);

        // Act
        Reservation result = reservationService.reserve(testReservation, testUser);

        // Assert
        assertNotNull(result);
        assertEquals(testUser, testReservation.getUser());
        assertEquals(testReservableRoom, testReservation.getReservableRoom());
        verify(reservationPort, times(1)).save(testReservation);
    }

    @Test
    void testReserve_予約可能会議室が存在しない場合() {
        // Arrange
        when(reservableRoomPort.findOneForUpdateByReservableRoomId(anyInt(), any(LocalDate.class)))
                .thenReturn(null);

        // Act & Assert
        ReservationService.UnavailableReservationException exception = assertThrows(
                ReservationService.UnavailableReservationException.class,
                () -> reservationService.reserve(testReservation, testUser)
        );
        assertEquals("指定の日付・部屋の組合わせは予約できません。", exception.getMessage());
        verify(reservationPort, never()).save(any());
    }

    @Test
    void testReserve_時間が重複する場合() {
        // Arrange
        when(reservableRoomPort.findOneForUpdateByReservableRoomId(anyInt(), any(LocalDate.class)))
                .thenReturn(testReservableRoom);

        // 既存の予約（11:00-13:00）
        Reservation existingReservation = new Reservation();
        existingReservation.setStartTime(LocalTime.of(11, 0));
        existingReservation.setEndTime(LocalTime.of(13, 0));

        when(reservationPort.findByReservableRoomOrderByStartTimeAsc(anyInt(), any(LocalDate.class)))
                .thenReturn(Arrays.asList(existingReservation));

        // Act & Assert
        ReservationService.AlreadyReservedException exception = assertThrows(
                ReservationService.AlreadyReservedException.class,
                () -> reservationService.reserve(testReservation, testUser)
        );
        assertEquals("入力の時間帯はすでに予約済みです。", exception.getMessage());
        verify(reservationPort, never()).save(any());
    }

    @Test
    void testReserve_時間が重複しない場合() {
        // Arrange
        when(reservableRoomPort.findOneForUpdateByReservableRoomId(anyInt(), any(LocalDate.class)))
                .thenReturn(testReservableRoom);

        // 既存の予約（8:00-9:00）- 重複しない
        Reservation existingReservation = new Reservation();
        existingReservation.setStartTime(LocalTime.of(8, 0));
        existingReservation.setEndTime(LocalTime.of(9, 0));

        when(reservationPort.findByReservableRoomOrderByStartTimeAsc(anyInt(), any(LocalDate.class)))
                .thenReturn(Arrays.asList(existingReservation));
        when(reservationPort.save(any(Reservation.class))).thenReturn(testReservation);

        // Act
        Reservation result = reservationService.reserve(testReservation, testUser);

        // Assert
        assertNotNull(result);
        verify(reservationPort, times(1)).save(testReservation);
    }

    @Test
    void testCancel_成功() {
        // Arrange
        Integer reservationId = 100;
        when(reservationPort.findById(reservationId)).thenReturn(testReservation);

        // Act
        reservationService.cancel(reservationId, testUser);

        // Assert
        verify(reservationPort, times(1)).findById(reservationId);
        verify(reservationPort, times(1)).delete(reservationId);
    }

    @Test
    void testCancel_予約が存在しない場合() {
        // Arrange
        Integer reservationId = 999;
        when(reservationPort.findById(reservationId)).thenReturn(null);

        // Act & Assert
        ReservationService.ReservationNotFoundException exception = assertThrows(
                ReservationService.ReservationNotFoundException.class,
                () -> reservationService.cancel(reservationId, testUser)
        );
        assertEquals("予約が見つかりません。", exception.getMessage());
        verify(reservationPort, never()).delete(any());
    }

    @Test
    void testReserve_複数の既存予約との重複チェック() {
        // Arrange
        when(reservableRoomPort.findOneForUpdateByReservableRoomId(anyInt(), any(LocalDate.class)))
                .thenReturn(testReservableRoom);

        // 既存の予約リスト
        Reservation reservation1 = new Reservation();
        reservation1.setStartTime(LocalTime.of(8, 0));
        reservation1.setEndTime(LocalTime.of(9, 0)); // 重複しない

        Reservation reservation2 = new Reservation();
        reservation2.setStartTime(LocalTime.of(13, 0));
        reservation2.setEndTime(LocalTime.of(14, 0)); // 重複しない

        Reservation reservation3 = new Reservation();
        reservation3.setStartTime(LocalTime.of(11, 0));
        reservation3.setEndTime(LocalTime.of(13, 0)); // 重複する

        when(reservationPort.findByReservableRoomOrderByStartTimeAsc(anyInt(), any(LocalDate.class)))
                .thenReturn(Arrays.asList(reservation1, reservation2, reservation3));

        // Act & Assert
        ReservationService.AlreadyReservedException exception = assertThrows(
                ReservationService.AlreadyReservedException.class,
                () -> reservationService.reserve(testReservation, testUser)
        );
        assertEquals("入力の時間帯はすでに予約済みです。", exception.getMessage());
        verify(reservationPort, never()).save(any());
    }
}