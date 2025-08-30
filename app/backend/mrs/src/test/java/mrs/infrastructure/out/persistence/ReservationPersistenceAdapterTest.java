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
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class ReservationPersistenceAdapterTest {

    @Mock
    private ReservationMapper reservationMapper;

    @InjectMocks
    private ReservationPersistenceAdapter reservationPersistenceAdapter;

    private Reservation testReservation;

    @BeforeEach
    void setUp() {
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
        verify(reservationMapper, times(1)).findByRoomIdAndDate(roomId, date);
    }

    @Test
    void testFindByRoomIdAndDate_空のリスト() {
        // Arrange
        Integer roomId = 1;
        LocalDate date = LocalDate.of(2025, 1, 1);
        List<Reservation> emptyReservations = Collections.emptyList();
        
        when(reservationMapper.findByRoomIdAndDate(roomId, date)).thenReturn(emptyReservations);

        // Act
        List<Reservation> actualReservations = reservationPersistenceAdapter.findByRoomIdAndDate(roomId, date);

        // Assert
        assertTrue(actualReservations.isEmpty());
        verify(reservationMapper, times(1)).findByRoomIdAndDate(roomId, date);
    }

    @Test
    void testFindByReservableRoomOrderByStartTimeAsc() {
        // Arrange
        Integer roomId = 1;
        LocalDate date = LocalDate.of(2025, 1, 1);
        List<Reservation> expectedReservations = Arrays.asList(testReservation);
        
        when(reservationMapper.findByRoomIdAndDate(roomId, date)).thenReturn(expectedReservations);

        // Act
        List<Reservation> actualReservations = reservationPersistenceAdapter.findByReservableRoomOrderByStartTimeAsc(roomId, date);

        // Assert
        assertEquals(expectedReservations, actualReservations);
        verify(reservationMapper, times(1)).findByRoomIdAndDate(roomId, date);
    }

    @Test
    void testFindByReservableRoomOrderByStartTimeAsc_空のリスト() {
        // Arrange
        Integer roomId = 1;
        LocalDate date = LocalDate.of(2025, 1, 1);
        List<Reservation> emptyReservations = Collections.emptyList();
        
        when(reservationMapper.findByRoomIdAndDate(roomId, date)).thenReturn(emptyReservations);

        // Act
        List<Reservation> actualReservations = reservationPersistenceAdapter.findByReservableRoomOrderByStartTimeAsc(roomId, date);

        // Assert
        assertTrue(actualReservations.isEmpty());
        verify(reservationMapper, times(1)).findByRoomIdAndDate(roomId, date);
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
        verify(reservationMapper, times(1)).findById(reservationId);
    }

    @Test
    void testFindById_存在しない場合() {
        // Arrange
        Integer reservationId = 999;
        when(reservationMapper.findById(reservationId)).thenReturn(null);

        // Act
        Reservation actualReservation = reservationPersistenceAdapter.findById(reservationId);

        // Assert
        assertNull(actualReservation);
        verify(reservationMapper, times(1)).findById(reservationId);
    }

    @Test
    void testSave() {
        // Arrange
        doNothing().when(reservationMapper).insert(testReservation);

        // Act
        Reservation result = reservationPersistenceAdapter.save(testReservation);

        // Assert
        assertEquals(testReservation, result);
        verify(reservationMapper, times(1)).insert(testReservation);
    }

    @Test
    void testDelete() {
        // Arrange
        Integer reservationId = 1;
        doNothing().when(reservationMapper).deleteById(reservationId);

        // Act
        reservationPersistenceAdapter.delete(reservationId);

        // Assert
        verify(reservationMapper, times(1)).deleteById(reservationId);
    }

    @Test
    void testFindByRoomIdAndDate_複数の予約() {
        // Arrange
        Integer roomId = 1;
        LocalDate date = LocalDate.of(2025, 1, 1);
        
        Reservation reservation1 = new Reservation();
        reservation1.setReservationId(1);
        reservation1.setStartTime(LocalTime.of(9, 0));
        reservation1.setEndTime(LocalTime.of(10, 0));
        
        Reservation reservation2 = new Reservation();
        reservation2.setReservationId(2);
        reservation2.setStartTime(LocalTime.of(14, 0));
        reservation2.setEndTime(LocalTime.of(15, 0));
        
        List<Reservation> expectedReservations = Arrays.asList(reservation1, reservation2);
        when(reservationMapper.findByRoomIdAndDate(roomId, date)).thenReturn(expectedReservations);

        // Act
        List<Reservation> actualReservations = reservationPersistenceAdapter.findByRoomIdAndDate(roomId, date);

        // Assert
        assertEquals(2, actualReservations.size());
        assertEquals(expectedReservations, actualReservations);
        verify(reservationMapper, times(1)).findByRoomIdAndDate(roomId, date);
    }
}