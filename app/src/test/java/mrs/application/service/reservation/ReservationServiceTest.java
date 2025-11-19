package mrs.application.service.reservation;

import mrs.application.domain.model.auth.*;
import mrs.application.domain.model.reservation.*;
import mrs.application.domain.model.room.*;
import mrs.application.port.out.ReservableRoomPort;
import mrs.application.port.out.ReservationPort;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class ReservationServiceTest {
    private ReservationPort reservationPort;
    private ReservableRoomPort reservableRoomPort;
    private ReservationService service;

    @BeforeEach
    void setUp() {
        reservationPort = mock(ReservationPort.class);
        reservableRoomPort = mock(ReservableRoomPort.class);
        service = new ReservationService(reservationPort, reservableRoomPort);
    }

    @Test
    void 予約一覧を取得できる() {
        ReservableRoomId roomId = createReservableRoomId();
        List<Reservation> reservations = List.of(
            createReservation(1, LocalTime.of(9, 0), LocalTime.of(10, 0))
        );
        when(reservationPort.findByReservableRoom_ReservableRoomIdOrderByStartTimeAsc(roomId))
            .thenReturn(reservations);

        ReservationList result = service.findReservations(roomId);

        assertEquals(1, result.size());
    }

    @Test
    void 重複しない予約を作成できる() {
        Reservation newReservation = createReservation(1, LocalTime.of(9, 0), LocalTime.of(10, 0));
        ReservableRoom reservableRoom = createReservableRoom();
        
        when(reservableRoomPort.findOneForUpdateByReservableRoomId(newReservation.reservableRoomId()))
            .thenReturn(reservableRoom);
        when(reservationPort.findByReservableRoom_ReservableRoomIdOrderByStartTimeAsc(newReservation.reservableRoomId()))
            .thenReturn(List.of());

        Reservation result = service.reserve(newReservation);

        assertNotNull(result);
        verify(reservationPort).save(newReservation);
    }

    @Test
    void 重複する予約は作成できない() {
        Reservation existing = createReservation(1, LocalTime.of(9, 0), LocalTime.of(10, 0));
        Reservation overlapping = createReservation(2, LocalTime.of(9, 30), LocalTime.of(10, 30));
        ReservableRoom reservableRoom = createReservableRoom();
        
        when(reservableRoomPort.findOneForUpdateByReservableRoomId(overlapping.reservableRoomId()))
            .thenReturn(reservableRoom);
        when(reservationPort.findByReservableRoom_ReservableRoomIdOrderByStartTimeAsc(overlapping.reservableRoomId()))
            .thenReturn(List.of(existing));

        assertThrows(AlreadyReservedException.class, () -> service.reserve(overlapping));
    }

    @Test
    void 予約可能会議室が存在しない場合は例外が発生する() {
        Reservation reservation = createReservation(1, LocalTime.of(9, 0), LocalTime.of(10, 0));
        
        when(reservableRoomPort.findOneForUpdateByReservableRoomId(reservation.reservableRoomId()))
            .thenReturn(null);

        assertThrows(UnavailableReservationException.class, () -> service.reserve(reservation));
    }

    @Test
    void 予約をキャンセルできる() {
        Reservation reservation = createReservation(1, LocalTime.of(9, 0), LocalTime.of(10, 0));

        service.cancel(reservation);

        verify(reservationPort).delete(reservation);
    }

    @Test
    void 予約者は自分の予約をキャンセルできる() {
        Reservation reservation = createReservation(1, LocalTime.of(9, 0), LocalTime.of(10, 0));
        User user = new User(new UserId("user001"), new Password("pass"), new Name("山田", "太郎"), RoleName.USER);

        assertTrue(service.canCancel(reservation, user));
    }

    @Test
    void 管理者は任意の予約をキャンセルできる() {
        Reservation reservation = createReservation(1, LocalTime.of(9, 0), LocalTime.of(10, 0));
        User admin = new User(new UserId("admin"), new Password("pass"), new Name("管理", "太郎"), RoleName.ADMIN);

        assertTrue(service.canCancel(reservation, admin));
    }

    @Test
    void 他人の予約はキャンセルできない() {
        Reservation reservation = createReservation(1, LocalTime.of(9, 0), LocalTime.of(10, 0));
        User otherUser = new User(new UserId("user002"), new Password("pass"), new Name("鈴木", "花子"), RoleName.USER);

        assertFalse(service.canCancel(reservation, otherUser));
    }

    private ReservableRoomId createReservableRoomId() {
        return new ReservableRoomId(LocalDate.of(2024, 1, 15), new RoomId(1));
    }

    private Reservation createReservation(int id, LocalTime start, LocalTime end) {
        return new Reservation(
            new ReservationId(id),
            new ReservationTimeSlot(start, end),
            createReservableRoomId(),
            new UserId("user001")
        );
    }

    private ReservableRoom createReservableRoom() {
        return new ReservableRoom(
            createReservableRoomId(),
            new MeetingRoom(new RoomId(1), new RoomName("会議室A"))
        );
    }
}
