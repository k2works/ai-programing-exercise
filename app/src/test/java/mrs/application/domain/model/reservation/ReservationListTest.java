package mrs.application.domain.model.reservation;

import mrs.application.domain.model.auth.UserId;
import org.junit.jupiter.api.Test;
import java.time.LocalTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class ReservationListTest {

    @Test
    void 予約リストを作成できる() {
        Reservation reservation = createReservation(1, LocalTime.of(9, 0), LocalTime.of(10, 0));
        ReservationList list = new ReservationList(List.of(reservation));
        assertEquals(1, list.size());
    }

    @Test
    void 空の予約リストを作成できる() {
        ReservationList list = new ReservationList(List.of());
        assertEquals(0, list.size());
    }

    @Test
    void 予約リストから要素を取得できる() {
        Reservation reservation = createReservation(1, LocalTime.of(9, 0), LocalTime.of(10, 0));
        ReservationList list = new ReservationList(List.of(reservation));
        assertEquals(reservation, list.get(0));
    }

    private Reservation createReservation(int id, LocalTime start, LocalTime end) {
        return new Reservation(
            new ReservationId(id),
            new ReservationTimeSlot(start, end),
            TestDataFactory.createReservableRoomId(),
            new UserId("user001")
        );
    }
}
