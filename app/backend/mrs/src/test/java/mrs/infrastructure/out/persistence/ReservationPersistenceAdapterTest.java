package mrs.infrastructure.out.persistence;

import mrs.application.domain.model.reservation.Reservation;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class ReservationPersistenceAdapterTest {
    
    static {
        // Disable Spring Boot during unit tests
        System.setProperty("spring.main.web-application-type", "none");
        System.setProperty("spring.autoconfigure.exclude", "org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration");
    }

    @Mock
    private ReservationMapper reservationMapper;

    @InjectMocks
    private ReservationPersistenceAdapter reservationPersistenceAdapter;

    private Reservation testReservation;

    @BeforeEach
    void setUp() {
        // モックをリセット
        reset(reservationMapper);
        
        testReservation = new Reservation();
        testReservation.setReservationId(1);
        testReservation.setStartTime(LocalTime.of(10, 0));
        testReservation.setEndTime(LocalTime.of(12, 0));
    }

    @Test
    void testFindByRoomIdAndDate() {
        // Arrange
        Integer roomId = 1;
        LocalDate date = LocalDate.of(2025, 1, 1);
        List<Reservation> expectedReservations = Arrays.asList(testReservation);
        
        when(reservationMapper.findByRoomIdAndDate(roomId, date)).thenReturn(expectedReservations);

        // Act
        List<Reservation> actualReservations = reservationPersistenceAdapter.findByRoomIdAndDate(roomId, date);

        // Assert
        assertEquals(expectedReservations, actualReservations);
    }

    @Test
    void testFindById() {
        // Arrange
        Integer reservationId = 1;
        when(reservationMapper.findById(reservationId)).thenReturn(testReservation);

        // Act
        Reservation actualReservation = reservationPersistenceAdapter.findById(reservationId);

        // Assert
        assertEquals(testReservation, actualReservation);
    }

    @Test
    void testSave() {
        // Arrange
        doNothing().when(reservationMapper).insert(testReservation);
        
        // Act
        Reservation result = reservationPersistenceAdapter.save(testReservation);

        // Assert
        assertEquals(testReservation, result);
    }

    @Test
    void testDelete() {
        // Arrange
        Integer reservationId = 1;
        doNothing().when(reservationMapper).deleteById(reservationId);

        // Act & Assert
        assertDoesNotThrow(() -> reservationPersistenceAdapter.delete(reservationId));
    }
}