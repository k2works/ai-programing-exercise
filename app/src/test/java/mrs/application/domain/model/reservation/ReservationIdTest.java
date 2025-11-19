package mrs.application.domain.model.reservation;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class ReservationIdTest {

    @Test
    void 予約IDを作成できる() {
        ReservationId reservationId = new ReservationId(1);
        assertEquals(1, reservationId.value());
    }

    @Test
    void 同じ値のReservationIdは等しい() {
        ReservationId id1 = new ReservationId(1);
        ReservationId id2 = new ReservationId(1);
        assertEquals(id1, id2);
        assertEquals(id1.hashCode(), id2.hashCode());
    }
}
