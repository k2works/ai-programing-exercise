package mrs.application.domain.model.reservation;

import org.junit.jupiter.api.Test;
import java.time.LocalTime;

import static org.junit.jupiter.api.Assertions.*;

class ReservationTimeSlotTest {

    @Test
    void 予約時間帯を作成できる() {
        LocalTime start = LocalTime.of(9, 0);
        LocalTime end = LocalTime.of(10, 0);
        ReservationTimeSlot slot = new ReservationTimeSlot(start, end);
        assertEquals(start, slot.startTime());
        assertEquals(end, slot.endTime());
    }

    @Test
    void nullの開始時刻は作成できない() {
        assertThrows(IllegalArgumentException.class, 
            () -> new ReservationTimeSlot(null, LocalTime.of(10, 0)));
    }

    @Test
    void nullの終了時刻は作成できない() {
        assertThrows(IllegalArgumentException.class, 
            () -> new ReservationTimeSlot(LocalTime.of(9, 0), null));
    }

    @Test
    void 終了時刻が開始時刻より前の場合は作成できない() {
        assertThrows(IllegalArgumentException.class, 
            () -> new ReservationTimeSlot(LocalTime.of(10, 0), LocalTime.of(9, 0)));
    }

    @Test
    void 重複する時間帯を検出できる() {
        ReservationTimeSlot slot1 = new ReservationTimeSlot(LocalTime.of(9, 0), LocalTime.of(10, 0));
        ReservationTimeSlot slot2 = new ReservationTimeSlot(LocalTime.of(9, 30), LocalTime.of(10, 30));
        assertTrue(slot1.overlap(slot2));
    }

    @Test
    void 重複しない時間帯を検出できる() {
        ReservationTimeSlot slot1 = new ReservationTimeSlot(LocalTime.of(9, 0), LocalTime.of(10, 0));
        ReservationTimeSlot slot2 = new ReservationTimeSlot(LocalTime.of(10, 0), LocalTime.of(11, 0));
        assertFalse(slot1.overlap(slot2));
    }

    @Test
    void 完全に含まれる時間帯は重複と判定される() {
        ReservationTimeSlot slot1 = new ReservationTimeSlot(LocalTime.of(9, 0), LocalTime.of(12, 0));
        ReservationTimeSlot slot2 = new ReservationTimeSlot(LocalTime.of(10, 0), LocalTime.of(11, 0));
        assertTrue(slot1.overlap(slot2));
    }
}
