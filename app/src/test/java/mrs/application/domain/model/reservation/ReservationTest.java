package mrs.application.domain.model.reservation;

import mrs.application.domain.model.auth.UserId;
import org.junit.jupiter.api.Test;
import java.time.LocalTime;

import static org.junit.jupiter.api.Assertions.*;

class ReservationTest {

    @Test
    void 予約を作成できる() {
        ReservationId id = new ReservationId(1);
        ReservationTimeSlot timeSlot = new ReservationTimeSlot(LocalTime.of(9, 0), LocalTime.of(10, 0));
        ReservableRoomId roomId = TestDataFactory.createReservableRoomId();
        UserId userId = new UserId("user001");

        Reservation reservation = new Reservation(id, timeSlot, roomId, userId);

        assertEquals(id, reservation.reservationId());
        assertEquals(timeSlot, reservation.timeSlot());
        assertEquals(roomId, reservation.reservableRoomId());
        assertEquals(userId, reservation.userId());
    }

    @Test
    void 重複する予約を検出できる() {
        ReservationTimeSlot timeSlot1 = new ReservationTimeSlot(LocalTime.of(9, 0), LocalTime.of(10, 0));
        ReservationTimeSlot timeSlot2 = new ReservationTimeSlot(LocalTime.of(9, 30), LocalTime.of(10, 30));
        ReservableRoomId roomId = TestDataFactory.createReservableRoomId();

        Reservation reservation1 = new Reservation(new ReservationId(1), timeSlot1, roomId, new UserId("user001"));
        Reservation reservation2 = new Reservation(new ReservationId(2), timeSlot2, roomId, new UserId("user002"));

        assertTrue(reservation1.overlap(reservation2));
    }

    @Test
    void 重複しない予約を検出できる() {
        ReservationTimeSlot timeSlot1 = new ReservationTimeSlot(LocalTime.of(9, 0), LocalTime.of(10, 0));
        ReservationTimeSlot timeSlot2 = new ReservationTimeSlot(LocalTime.of(10, 0), LocalTime.of(11, 0));
        ReservableRoomId roomId = TestDataFactory.createReservableRoomId();

        Reservation reservation1 = new Reservation(new ReservationId(1), timeSlot1, roomId, new UserId("user001"));
        Reservation reservation2 = new Reservation(new ReservationId(2), timeSlot2, roomId, new UserId("user002"));

        assertFalse(reservation1.overlap(reservation2));
    }

    @Test
    void 異なる会議室の予約は重複しない() {
        ReservationTimeSlot timeSlot = new ReservationTimeSlot(LocalTime.of(9, 0), LocalTime.of(10, 0));
        ReservableRoomId roomId1 = TestDataFactory.createReservableRoomId(1);
        ReservableRoomId roomId2 = TestDataFactory.createReservableRoomId(2);

        Reservation reservation1 = new Reservation(new ReservationId(1), timeSlot, roomId1, new UserId("user001"));
        Reservation reservation2 = new Reservation(new ReservationId(2), timeSlot, roomId2, new UserId("user002"));

        assertFalse(reservation1.overlap(reservation2));
    }
}
