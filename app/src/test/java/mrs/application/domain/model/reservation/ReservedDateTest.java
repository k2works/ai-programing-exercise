package mrs.application.domain.model.reservation;

import org.junit.jupiter.api.Test;
import java.time.LocalDate;

import static org.junit.jupiter.api.Assertions.*;

class ReservedDateTest {

    @Test
    void 予約日を作成できる() {
        LocalDate date = LocalDate.of(2024, 1, 15);
        ReservedDate reservedDate = new ReservedDate(date);
        assertEquals(date, reservedDate.value());
    }

    @Test
    void nullの予約日は作成できない() {
        assertThrows(IllegalArgumentException.class, () -> new ReservedDate(null));
    }

    @Test
    void 同じ値のReservedDateは等しい() {
        LocalDate date = LocalDate.of(2024, 1, 15);
        ReservedDate date1 = new ReservedDate(date);
        ReservedDate date2 = new ReservedDate(date);
        assertEquals(date1, date2);
        assertEquals(date1.hashCode(), date2.hashCode());
    }
}
