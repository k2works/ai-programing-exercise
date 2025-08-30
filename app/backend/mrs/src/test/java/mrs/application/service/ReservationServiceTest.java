package mrs.application.service;

import mrs.application.domain.model.auth.User;
import mrs.application.domain.model.reservation.Reservation;
import mrs.application.domain.model.room.ReservableRoom;
import mrs.application.exception.ReservationNotFoundException;
import mrs.application.exception.UnavailableReservationException;
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
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class ReservationServiceTest {
    
    static {
        // Disable Spring Boot during unit tests
        System.setProperty("spring.main.web-application-type", "none");
        System.setProperty("spring.autoconfigure.exclude", "org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration");
    }

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
        // モックをリセット
        reset(reservationPort, reservableRoomPort);
        
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
    }

    @Test
    void testReserve_成功() {
        // Arrange
        when(reservableRoomPort.findOneForUpdateByReservableRoomId(1, LocalDate.of(2025, 1, 1)))
                .thenReturn(testReservableRoom);
        when(reservationPort.findByReservableRoomOrderByStartTimeAsc(1, LocalDate.of(2025, 1, 1)))
                .thenReturn(Collections.emptyList());
        when(reservationPort.save(testReservation)).thenReturn(testReservation);

        // Act
        Reservation result = reservationService.reserve(testReservation, testUser);

        // Assert
        assertNotNull(result);
        assertEquals(testReservation, result);
    }

    @Test
    void testReserve_予約可能会議室が存在しない場合() {
        // Arrange
        when(reservableRoomPort.findOneForUpdateByReservableRoomId(1, LocalDate.of(2025, 1, 1)))
                .thenReturn(null);

        // Act & Assert
        UnavailableReservationException exception = assertThrows(
                UnavailableReservationException.class,
                () -> reservationService.reserve(testReservation, testUser)
        );
        assertEquals("指定の日付・部屋の組合わせは予約できません。", exception.getMessage());
    }

    @Test
    void testCancel_成功() {
        // Arrange
        Integer reservationId = 100;
        when(reservationPort.findById(reservationId)).thenReturn(testReservation);
        doNothing().when(reservationPort).delete(reservationId);

        // Act & Assert
        assertDoesNotThrow(() -> reservationService.cancel(reservationId, testUser));
    }

    @Test
    void testCancel_予約が存在しない場合() {
        // Arrange
        Integer reservationId = 999;
        when(reservationPort.findById(reservationId)).thenReturn(null);

        // Act & Assert
        ReservationNotFoundException exception = assertThrows(
                ReservationNotFoundException.class,
                () -> reservationService.cancel(reservationId, testUser)
        );
        assertEquals("予約が見つかりません。", exception.getMessage());
    }
}