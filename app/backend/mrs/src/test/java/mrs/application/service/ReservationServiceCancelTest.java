package mrs.application.service;

import mrs.application.domain.model.auth.User;
import mrs.application.domain.model.reservation.Reservation;
import mrs.application.domain.model.room.ReservableRoom;
import mrs.application.port.out.ReservableRoomPort;
import mrs.application.port.out.ReservationPort;
import mrs.common.exception.ReservationNotFoundException;
import mrs.common.exception.UnauthorizedAccessException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDate;
import java.time.LocalTime;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class ReservationServiceCancelTest {

    @Mock
    private ReservationPort reservationPort;

    @Mock
    private ReservableRoomPort reservableRoomPort;

    @InjectMocks
    private ReservationService reservationService;

    private User owner;
    private User otherUser;
    private User admin;
    private Reservation reservation;

    @BeforeEach
    void setUp() {
        owner = new User();
        owner.setUserId("owner");
        owner.setName("Owner User");
        owner.setRole("USER");

        otherUser = new User();
        otherUser.setUserId("other");
        otherUser.setName("Other User");
        otherUser.setRole("USER");

        admin = new User();
        admin.setUserId("admin");
        admin.setName("Admin User");
        admin.setRole("ADMIN");

        ReservableRoom reservableRoom = new ReservableRoom();
        reservableRoom.setRoomId(1);
        reservableRoom.setReservableDate(LocalDate.of(2025, 9, 1));

        reservation = new Reservation();
        reservation.setReservationId(1);
        reservation.setStartTime(LocalTime.of(9, 0));
        reservation.setEndTime(LocalTime.of(10, 0));
        reservation.setReservableRoom(reservableRoom);
        reservation.setUser(owner);
    }

    @Test
    void testCancelReservation_OwnerCanCancel() {
        when(reservationPort.findById(1)).thenReturn(reservation);

        assertDoesNotThrow(() -> reservationService.cancel(1, owner));
        verify(reservationPort, times(1)).delete(1);
    }

    @Test
    void testCancelReservation_AdminCanCancelAny() {
        when(reservationPort.findById(1)).thenReturn(reservation);

        assertDoesNotThrow(() -> reservationService.cancel(1, admin));
        verify(reservationPort, times(1)).delete(1);
    }

    @Test
    void testCancelReservation_OtherUserCannotCancel() {
        when(reservationPort.findById(1)).thenReturn(reservation);

        UnauthorizedAccessException exception = assertThrows(
            UnauthorizedAccessException.class,
            () -> reservationService.cancel(1, otherUser)
        );
        assertEquals("他人の予約はキャンセルできません。", exception.getMessage());
        verify(reservationPort, never()).delete(anyInt());
    }

    @Test
    void testCancelReservation_NotFound() {
        when(reservationPort.findById(999)).thenReturn(null);

        ReservationNotFoundException exception = assertThrows(
            ReservationNotFoundException.class,
            () -> reservationService.cancel(999, owner)
        );
        assertEquals("予約が見つかりません。", exception.getMessage());
        verify(reservationPort, never()).delete(anyInt());
    }
}